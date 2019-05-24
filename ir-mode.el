(eval-when-compile (require 'cl-lib))
(require 'smie nil 'noerror)

(defgroup ir ()
  "Editing IR code."
  :group 'languages
  :prefix "ir-")

(defcustom ir-indent-level 4
  "Basic indentation step for IR code."
  :type 'integer)

(defvar ir-mode-hook nil
  "Run upon entering `ir-mode'.
This is a good place to put your preferred key bindings.")

(defvar ir-smie-verbose-p nil
  "Emit context information about the current syntax state.")

(defvar ir-use-smie t)

(defvar ir-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    (modify-syntax-entry ?\# "w" st)
    (mapc (lambda (c) (modify-syntax-entry c "." st)) ",;:=")
    st)
  "The syntax table used in `ir-mode'.")

(defconst ir-keywords-regexp
  (rx symbol-start
      (or "#region" "#endregion" "bb" "preds" "entry" "exit")
      symbol-end)
  "A regexp that matches any and all keywords of IR.")

(defconst ir-font-lock-symbols-alist
  '(("->"  . ?→)
    ("=>"  . ?⇒)
    ("<-"  . ?←)
    ("<>"  . ?≠)
    (">="  . ?≥)
    ("<="  . ?≤)))

;; (defconst ir-smie-grammar
;;   (smie-prec2->grammar
;;    (smie-merge-prec2s
;;     (smie-bnf->prec2
;;      '((id)
;;        (region (region-header "__$__" basic-blocks "__$__" region-end))
;;        (region-header ("REGION-BEGIN" id ":" region-props))
;;        (region-props (region-props ";" region-props) (region-prop))
;;        (region-prop ("PM64") ("PM32") ("G2") ("eva" "=" id) ("ginsts" "=" id))
;;        (region-end ("REGION-END"))
;;        (basic-blocks (basic-blocks "__$__" basic-blocks) (basic-block))
;;        (basic-block
;;         (bb-header "__$__" instructions "__$__" edges)
;;         (bb-header "__$__" instructions)
;;         (bb-header "__$__" edges)
;;         (bb-header))
;;        (bb-header ("BASIC-BLOCK" id ":" bb-props))
;;        (bb-props (bb-props ";" bb-props) (bb-prop))
;;        (bb-prop ("entry") ("exit"))
;;        (instructions (instructions "__$__" instructions) (instruction))
;;        (instruction
;;         (id "." opcode args ";" inst-gva)
;;         (id "." opcode args "->" args ";" inst-gva))
;;        (opcode (id))
;;        (args (args "," args) (operand))
;;        (operand (imm) (reg))
;;        (imm ("i:" id))
;;        (reg (id))
;;        (inst-gva ("gva:" id))
;;        (edges (edges "," edges) (edge))
;;        (edge ("BB-EDGE")))
;;      '((assoc "__$__"))
;;      '((assoc ":"))
;;      '((assoc ";"))
;;      '((assoc ","))
;;      '((assoc "="))))))

(defconst ir-smie-grammar
  (smie-prec2->grammar
   (smie-merge-prec2s
    (smie-bnf->prec2
     '((id)
       (prop (id ":" id) (id "=" id))
       (region (region-header "__$__" (basic-blocks) "__$__" region-end))
       (region-header
        ;; ("REGION-BEGIN" id ":" region-props)
        ("REGION-BEGIN" id))
       (region-prop ("PM64") ("PM32") ("G2") (prop))
       (region-props (region-props ";" region-props) (region-prop))
       (region-end ("REGION-END"))
       (basic-blocks (basic-blocks "__$__" basic-blocks) (basic-block))
       (basic-block
        (bb-header "__$__" insts "__$__" bb-edges)
        (bb-header))
       (bb-header ("BASIC-BLOCK" id))
       (bb-edges (bb-edges "," bb-edges) (bb-edge))
       (bb-edge ("BB-EDGE"))
       (insts (insts "__$__" insts) (inst))
       (inst (id)))
     '((assoc "__$__"))
     '((assoc ","))
     '((assoc ":"))
     '((assoc ";"))))))


(defmacro ir-smie-debug (message &rest format-args)
  `(progn
     (when ir-smie-verbose-p
       (message (format ,message ,@format-args)))
     nil))

(defmacro get-ps-elem (ppss &key name)
  (ecase name
    (:paren-depth `(nth 0 ,ppss))
    (:paren-inner-start `(nth 1 ,ppss))
    (:subexp-term-start `(nth 2 ,ppss))
    (:inside-string-p `(nth 3 ,ppss))
    (:non-nest-comment-p `(nth 4 ,ppss))
    (:after-quote-p `(nth 5 ,ppss))
    (:min-pren-depth `(nth 6 ,ppss))
    (:comment-kind `(nth 7 ,ppss))
    (:string-or-comment-start `(nth 8 ,ppss))))

(defvar ir-smie-empty-line
  (rx (and bol (0+ space) eol)))

(defvar ir-smie-spaces-till-end-of-line
  (rx (and (0+ space) eol)))

(defvar ir-smie-region-begin-regexp
  (rx (and word-start "#region" word-end)))

(defvar ir-smie-region-end-regexp
  (rx (and "#endregion" word-end)))

(defvar ir-smie-comment-start
  (rx "//"))

(defvar ir-smie-bb-begin-regexp
  (rx (and word-start "bb" word-end)))

(defvar ir-smie-bb-edge-regexp
  (rx (and word-start (in "TF") "[b" (1+ digit) (0+ space)
           "->" (0+ space) "b" (1+ digit) (in "]"))))

(defvar ir-smie-right-arrow-regexp (rx "->"))

(defun ir-smie-line-is-region-begin ()
  (save-excursion
   (beginning-of-line)
   (looking-at ir-smie-region-begin-regexp)))

(defun ir-smie-last-line-is-region-begin ()
  (save-excursion
   (forward-line -1)
   (beginning-of-line)
   (looking-at ir-smie-region-begin-regexp)))

(defun ir-smie-last-line-is-empty ()
  (save-excursion
   (beginning-of-line)
   (unless (bobp)
     (forward-line -1)
     (looking-at ir-smie-empty-line))))

(defun ir-smie-line-is-bb-begin ()
  (save-excursion
   (beginning-of-line)
   (looking-at ir-smie-bb-begin-regexp)))

(defun ir-smie-last-line-is-bb-begin ()
  (save-excursion
   (forward-line -1)
   (beginning-of-line)
   (looking-at ir-smie-bb-begin-regexp)))

(defun ir-smie-line-is-bb-edge ()
  (save-excursion
   (beginning-of-line)
   (looking-at ir-smie-bb-edge-regexp)))

(defun ir-smie-last-line-is-bb-edge ()
  (save-excursion
   (forward-line -1)
   (beginning-of-line)
   (looking-at ir-smie-bb-edge-regexp)))

(defun verbose-ir-smie-forward-token ()
  (let ((tok (ir-smie-forward-token)))
    (ir-smie-debug "forward token: '%s'" tok)
    ;; (message "p: %d" (point))
    tok))

(defun ir-smie-forward-token ()
  ;; (ir-smie-debug "ir-smie-forward-token")
  (let ((pos (line-end-position)))
    (forward-comment (point-max))
    ;; (message "pos: %d, point: %d, eol: %d" pos (point) (line-end-position))
    (cond
      ((> (point) pos)
       "__$__")
      ((looking-at ir-smie-region-begin-regexp)
       ;; (message "(looking-at ir-smie-region-begin-regexp)")
       (goto-char (match-end 0))
       "REGION-BEGIN")
      ((looking-at ir-smie-region-end-regexp)
       (goto-char (match-end 0))
       "REGION-END")
      ((looking-at ir-smie-bb-begin-regexp)
       (goto-char (match-end 0))
       "BASIC-BLOCK")
      ((looking-at ir-smie-bb-edge-regexp)
       (goto-char (match-end 0))
       "BB-EDGE")
      (t
       ;; (message "//**//")
       (buffer-substring-no-properties
        (point)
        (progn (or (/= 0 (skip-syntax-forward "'w_"))
                   (skip-syntax-forward "."))
               (point)))))))

(defun verbose-ir-smie-backward-token ()
  (let ((tok (ir-smie-backward-token)))
    (ir-smie-debug "backward token: '%s'" tok)
    tok))

(defun ir-smie-backward-token ()
  ;; (ir-smie-debug "ir-smie-backward-token")
  (let ((pos (point)))
    (forward-comment (- pos))
    ;; (message "pos: %d, point: %d, eol: %d" pos (point) (line-end-position))
    (cond
      ((> pos (line-end-position))
       "__$__")
      ((looking-back ir-smie-region-begin-regexp (- (point) 8))
       (goto-char (match-beginning 0))
       "REGION-BEGIN")
      ((looking-back ir-smie-region-end-regexp (- (point) 11))
       (goto-char (match-beginning 0))
       "REGION-END")
      ((looking-back ir-smie-bb-edge-regexp (- (point) 15))
       (goto-char (match-beginning 0))
       "BB-EDGE")
      ((looking-back ir-smie-bb-begin-regexp (- (point) 3))
       (goto-char (match-beginning 0))
       "BASIC-BLOCK")
      (t
       ;; (message "!!!!")
       (buffer-substring-no-properties
        (point)
        (progn (or (/= 0 (skip-syntax-backward "'w_"))
                   (skip-syntax-backward "."))
               (point)))))))

(defun verbose-ir-smie-rules (kind token)
  (let ((value (ir-smie-rules kind token)))
    (ir-smie-debug "rule (%s . '%s'): sibling-p: %s, parent: %s, hanging: %s -> %s" kind token
                   (ignore-errors (smie-rule-sibling-p))
                   (ignore-errors smie--parent)
                   (ignore-errors (smie-rule-hanging-p))
                   value)
    value))

(defun ir-smie-rules (kind token)
  ;; (ir-smie-debug "(ir-smie-rules '%s' '%s')" kind token)
  (pcase (cons kind token)
         (`(:elem . basic) ir-indent-level)
         ;; (`(:elem . args) 0)
         (`(:before . "REGION-BEGIN") `(column . 0))
         (`(:before . "REGION-END") `(column . 0))
         (`(:before . "BASIC-BLOCK") `(column . 0))
         (`(:before . "BB-EDGE") `(column . 0))
         (`(:before . "__$__") `(column . 0))
         (`(:after . "__$__")
           ;; (ir-smie-debug "aaaaa")
           (cond
             ((smie-rule-hanging-p)
              (cond
                ((ir-smie-line-is-region-begin)
                 (ir-smie-debug "after __$__, hanging: region begin")
                 `(column . 0))
                ((ir-smie-line-is-bb-begin)
                 (ir-smie-debug "after __$__, hanging: bb begin")
                 `(column . ,ir-indent-level))
                ((ir-smie-line-is-bb-edge)
                 (ir-smie-debug "after __$__, hanging: bb edge")
                 `(column . 0))
                (t
                 (ir-smie-debug "after __$__, hanging: default")
                 (smie-rule-parent))))
             (t
              (ir-smie-debug "after __$__, not hanging: default")
              (smie-rule-parent))))))

;;
(defvar ir-bookmark-last-modified-tick nil)
(make-variable-buffer-local 'ir-bookmark-last-modified-tick)

(defvar ir-bb-bookmark-regexp (rx (and bol (0+ space) "bb" (1+ space) (group (1+ numeric)) ":")))
(defvar ir-inst-bookmark-regexp (rx (and bol (0+ space) (group (1+ numeric)) "."
                                         (1+ space) (1+ (not (in space ";")))
                                         (0+ (not (in ";"))))))
(defvar ir-bb-edge-bookmark-regexp
  (rx (and word-start (group (in "TF")) "[b" (group (1+ digit)) (0+ space)
           "->" (0+ space) "b" (group (1+ digit)) (in "]"))))

(defvar ir-mode-bookmark-alist ())
(make-variable-buffer-local 'ir-mode-bookmark-alist)

(defvar ir-mode-digraph ())
(make-variable-buffer-local 'ir-mode-digraph)

(defun ir-bookmark-name (record)
  (car record))

(defun ir-get-bookmark (name-or-record &optional no-error)
  (cond
    ((consp name-or-record) name-or-record)
    ((stringp name-or-record)
     (or (assoc-string name-or-record ir-mode-bookmark-alist)
         (unless no-error (error "invalid bookmark %s" name-or-record))))))

(defun ir-bookmark-get-record (name-or-record)
  (cdr (ir-get-bookmark name-or-record)))

(defun ir-bookmark-set-name (name-or-record newname)
  (setcar (ir-get-bookmark name-or-record) newname))

(defun ir-bookmark-get-prop (name-or-record prop)
  (cdr (assq prop (ir-bookmark-get-record name-or-record))))

(defun ir-bookmark-set-prop (name-or-record prop val)
  (let ((cell (assq prop (ir-bookmark-get-record name-or-record))))
    (if cell
        (setcdr cell vall)
        (nconc (ir-bookmark-get-record name-or-record)
               (list (cons prop val))))))

(defun ir-bookmark-get-position (name-or-record)
  (ir-bookmark-get-prop name-or-record 'position))

(defun ir-bookmark-set-position (name-or-record position)
  (ir-bookmark-set-prop name-or-record 'position position))

(defun ir-bookmark-make-default-record (&optional position)
  `((position . ,(or position (point)))))

(defun ir-bookmark-make-record ()
  `(nil . ,(ir-bookmark-make-default-record)))

(defun ir-bookmark-store (name alist no-overwrite)
  ;; (message "[ir-bookmark-store] name: %s, alist: %s, no-overwrite: %s"
  ;;          name alist no-overwrite)
  (if (and (not no-overwrite)
           (ir-get-bookmark name 'no-error))
      (setcdr (ir-get-bookmark name) alist)
      (push (cons name alist) ir-mode-bookmark-alist)))

(defun ir-bookmark-set-bookmark-internal (name overwrite-or-push)
  (let ((record (ir-bookmark-make-record)))
    (cond
      ((eq overwrite-or-push nil)
       (if (ir-get-bookmark name 'no-error)
           (error "bookmark named \"%s\" already exists." name)
           (ir-bookmark-store name (cdr record) nil)))
      ((eq overwrite-or-push 'overwrite)
       (ir-bookmark-store name (cdr record) nil))
      ((eq overwrite-or-push 'push)
       (ir-bookmark-store name (cdr record) 'no-overwrite))
      (t (error "Illegal value of `overwrite-or-push': %s" overwrite-or-push)))))

(defvar ir-bookmark-inst-prefix "inst-")
(defvar ir-bookmark-bb-prefix "bb ")

(defun ir-mode-update-inst-bookmarks ()
  (save-excursion
   (beginning-of-buffer)
   (while (re-search-forward ir-inst-bookmark-regexp nil 'no-error)
          (goto-char (match-beginning 1))
          (ir-bookmark-set-bookmark-internal (concat ir-bookmark-inst-prefix
                                                     (match-string-no-properties 1))
                                             nil)
          (goto-char (match-end 0)))))

(defun ir-mode-store-edge-internal (place type bb-num)
  (pcase type
    ("T" (setcar place bb-num))
    ("F" (setcdr place (list bb-num)))
    (t (error "illegal edge type %s" type))))

(defun ir-mode-find-edges (bb-num)
  (save-excursion
   (let ((next-bb-pos nil)
         (edges (list nil nil)))
     (when (save-excursion (re-search-forward ir-bb-bookmark-regexp nil 'no-error))
       (setq next-bb-pos (match-beginning 0)))
     ;; (message "next-bb-pos %d (%d)" (or next-bb-pos -1) (point))
     (when (re-search-forward ir-bb-edge-bookmark-regexp nil 'no-error)
       (goto-char (match-beginning 0))
       ;; (message "point %d" (point))
       (when (or (null next-bb-pos) (> next-bb-pos (point)))
         (let ((edge-src (string-to-number (match-string-no-properties 2) 10))
               (edge-dst (string-to-number (match-string-no-properties 3) 10)))
           (when (/= edge-src bb-num)
             (error "incorrect edge source %d (expexting %d)" edge-src bb-num))
           (ir-mode-store-edge-internal edges (match-string-no-properties 1) edge-dst)
           (goto-char (match-end 0))
           (skip-chars-forward " ,")
           (when (looking-at ir-bb-edge-bookmark-regexp)
             (setq edge-src (string-to-number (match-string-no-properties 2) 10)
                   edge-dst (string-to-number (match-string-no-properties 3) 10))
             (when (/= edge-src bb-num)
               (error "incorrect edge source %d (expexting %d)" edge-src bb-num))
             (ir-mode-store-edge-internal edges (match-string-no-properties 1) edge-dst)))))
     edges)))

(defun ir-mode-update-bb-bookmarks ()
  (save-excursion
   (beginning-of-buffer)
   (while (re-search-forward ir-bb-bookmark-regexp nil 'no-error)
          (goto-char (match-beginning 0))
          (ir-bookmark-set-bookmark-internal (concat ir-bookmark-bb-prefix
                                                     (match-string-no-properties 1))
                                             nil)
          (goto-char (match-end 0))
          (let* ((cur-bb-num (string-to-number (match-string-no-properties 1) 10))
                 (edges (ir-mode-find-edges cur-bb-num)))
            (push (list cur-bb-num edges) ir-mode-digraph)))))

(defun ir-mode-construct-dot-input ()
  (let ((str "digraph cfg {\n"))
    (dolist (elt (reverse ir-mode-digraph))
      (let ((bb-num (car elt))
            (edges (car (cdr elt))))
        (setq str (concat str
                          (pcase edges
                                 (`(nil nil) (format "  b%d;\n" bb-num))
                                 (`(nil ,e) (format "  b%d -> b%d;\n" bb-num e))
                                 (`(,e nil) (format "  b%d -> b%d;\n" bb-num e))
                                 (`(,et ,ef) (format "  b%d -> b%d;\n  b%d -> b%d;\n"
                                                     bb-num et bb-num ef)))))))
    (concat str "}")))

(defvar ir-mode-jump-stack ())
(make-local-variable 'ir-mode-jump-stack)

(defun ir-mode-bb-jump ()
  (interactive)
  (let ((word (current-word 'strict)))
    (cond
      ((string-match (rx (and bol "b" (group (1+ numeric)) eol)) word)
       (let ((pos (ir-bookmark-get-position (concat ir-bookmark-bb-prefix (match-string 1 word)))))
         (when pos
           (push (point) ir-mode-jump-stack)
           (goto-char pos)))))))

(defun ir-mode-jump-back ()
  (interactive)
  (let ((new-pos (pop ir-mode-jump-stack)))
    (if new-pos
        (goto-char new-pos)
        (message "jump stack is empty."))))

;;;###autoload
(defun ir-mode-update-bookmarks ()
  (interactive)
  (let ((current-mod-tick (buffer-modified-tick)))
    (unless (eq current-mod-tick ir-bookmark-last-modified-tick)
      (setq ir-mode-bookmark-alist ()
            ir-mode-digraph ())
      (ir-mode-update-inst-bookmarks)
      (ir-mode-update-bb-bookmarks)
      (setq ir-bookmark-last-modified-tick current-mod-tick))))

;;;###autoload
(defun ir-mode-goto-inst-bookmark (name)
  (interactive "sGoto-inst: \n")
  (let ((bm (ir-get-bookmark (concat ir-bookmark-inst-prefix name) 'no-error)))
    (if bm
        (goto-char (ir-bookmark-get-position bm))
        (error "unknown instruction: %s" name))))

;;;###autoload
(defun ir-mode-goto-bb-bookmark (name)
  (interactive "sGoto-bb: \n")
  (let ((bm (ir-get-bookmark (concat ir-bookmark-bb-prefix name) 'no-error)))
    (if bm
        (goto-char (ir-bookmark-get-position bm))
        (error "unknown basic block: b%s" name))))

;;;###autoload
(defun ir-mode-show-digraph ()
  (interactive)
  (save-excursion
   (let ((dot-input (ir-mode-construct-dot-input)))
     (with-current-buffer (get-buffer-create "ir-mode-cfg")
       (erase-buffer)
       (insert-image (create-image
                      (with-temp-buffer
                          (insert dot-input)
                        (call-process-region (point-min) (point-max) "dot" t t nil "-Tsvg")
                        (buffer-string))
                      'svg t)))))
  (switch-to-buffer "ir-mode-cfg"))

(defvar ir-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-[") 'ir-mode-bb-jump)
    (define-key map (kbd "M-]") 'ir-mode-jump-back)
    map)
  "The keymap used in `ir-mode'.")

;;;###autoload
(define-derived-mode ir-mode prog-mode "IR"
  "Major mode for IR."
  (toggle-truncate-lines nil)
  (set-syntax-table ir-mode-syntax-table)
  (setq-local comment-start "//")
  ;; (set (make-local-variable 'comment-start-skip) "//+[ \t]*")
  ;; (setq-local comment-end "\n")
  ;; (make-local-variable 'comment-end)
  (if ir-use-smie
      (smie-setup ir-smie-grammar 'verbose-ir-smie-rules
                  :forward-token 'verbose-ir-smie-forward-token
                  :backward-token 'verbose-ir-smie-backward-token))
  (ir-mode-update-bookmarks)
  (use-local-map ir-mode-map))

(provide 'ir-mode)
