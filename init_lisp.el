(setq blink-matching-paren nil)

(show-paren-mode 1)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(define-auto-insert 'lisp-mode '(insert ";;; -*- Mode: Lisp -*-\n"))

(my-require-and-eval (paredit paredit paredit)
  (defvar paredit-space-for-delimiter-predicates nil)

  (defun paredit-space-for-delimiter-p (endp delimiter)
    ;; If at the buffer limit, don't insert a space.  If there is a word,
    ;; symbol, other quote, or non-matching parenthesis delimiter (i.e. a
    ;; close when want an open the string or an open when we want to
    ;; close the string), do insert a space.
    (and (not (if endp (eobp) (bobp)))
         (memq (char-syntax (if endp (char-after) (char-before)))
               (list ?w ?_ ?\"
                     (let ((matching (matching-paren delimiter)))
                       (and matching (char-syntax matching)))
                     (and (not endp)
                          (eq ?\" (char-syntax delimiter))
                          ?\) )))
         (catch 'exit
           (dolist (predicate paredit-space-for-delimiter-predicates)
             (if (not (funcall predicate endp delimiter))
                 (throw 'exit nil)))
           t)))

  (defvar common-lisp-octothorpe-quotation-characters '(?P))
  (defvar common-lisp-octothorpe-parameter-parenthesis-characters '(?A))
  (defvar common-lisp-octothorpe-parenthesis-characters '(?+ ?- ?C))

  (defun paredit-space-for-delimiter-predicate-common-lisp (endp delimiter)
    (or endp
        (let ((case-fold-search t)
              (look
               (lambda (prefix characters n)
                 (looking-back
                  (concat prefix (regexp-opt (mapcar 'string characters)))
                  (- (point) n)))))
          (let ((oq common-lisp-octothorpe-quotation-characters)
                (op common-lisp-octothorpe-parenthesis-characters)
                (opp common-lisp-octothorpe-parameter-parenthesis-characters))
            (cond ((eq (char-syntax delimiter) ?\()
                   (and (not (funcall look "#" op 2))
                        (not (funcall look "#[0-9]*" opp 20))))
                  ((eq (char-syntax delimiter) ?\")
                   (not (funcall look "#" oq 2)))
                  (t t))))))

  (add-hook 'lisp-mode-hook
            (defun common-lisp-mode-hook-paredit ()
              (make-local-variable 'paredit-space-for-delimiter-predicates)
              (add-to-list 'paredit-space-for-delimiter-predicates
                           'paredit-space-for-delimiter-predicate-common-lisp))))

(my-require-and-eval (redshank redshank redshank))

(flet ((parens (mode)
         (when (featurep 'paredit)
           (add-hook mode 'paredit-mode))
         (when (featurep 'redshank)
           (add-hook mode 'redshank-mode))))
  (mapc 'parens '(emacs-lisp-mode-hook lisp-mode-hook)))

(setq slime-additional-font-lock-keywords nil)

(my-require-and-eval (slime slime)
  (defun load-slime ()
    (slime-setup '(slime-fancy
                   slime-sbcl-exts slime-scheme
                   slime-sprof
                   slime-asdf
                   slime-indentation
                   ;; slime-cover
                   )) ;; slime-gauche
    (let ((slime-fasls-directory (expand-file-name
                                  "~/lisp/fasls/from-slime/")))
      (make-directory slime-fasls-directory t)
      (setq slime-compile-file-options
            `(:fasl-directory ,slime-fasls-directory)))
    (make-directory "~/.config/emacs/" t)
    (setq
     lisp-indent-function 'common-lisp-indent-function
     slime-complete-symbol-function 'slime-fuzzy-complete-symbol
     slime-net-coding-system 'utf-8-unix
     slime-startup-animation nil
     slime-auto-select-connection 'always
     ;; common-lisp-hyperspec-root
     ;; (concat "file:/" (expand-file-name "~/doc/comp/lang/lisp/HyperSpec/"))
     inferior-lisp-program "/usr/bin/sbcl"
     slime-kill-without-query-p t
     slime-when-complete-filename-expand t
     slime-description-autofocus t
     slime-repl-history-remove-duplicates t
     slime-repl-history-trim-whitespaces t
     slime-fuzzy-explanation ""
     slime-repl-history-file "~/.config/emacs/slime-history.eld"
     slime-asdf-collect-notes t
     slime-inhibit-pipelining nil
     ;; slime-compilation-finished-hook 'slime-list-compiler-notes
     slime-load-failed-fasl 'always
     lisp-loop-indent-subclauses nil
     lisp-loop-indent-forms-like-keywords t
     lisp-lambda-list-keyword-parameter-alignment t
     slime-export-symbol-representation-auto t
     slime-export-save-file t
     slime-edit-uses-xrefs (remove :depends-on slime-edit-uses-xrefs)
     eldoc-echo-area-use-multiline-p nil)

    (when (boundp 'slime-repl-mode-map)
      (define-key slime-repl-mode-map "\C-c\C-u" 'slime-repl-delete-current-input)
      (define-key slime-repl-mode-map [tab] 'slime-complete-symbol))

    (define-key slime-mode-map "\C-c\M-i" 'slime-inspect-definition)
    (define-key slime-editing-map "\C-c\M-d" 'slime-disassemble-definition)
    (define-key slime-editing-map "\C-c\M-D" 'slime-disassemble-full-definition)

    ;; (substitute-key-definition 'slime-xref-next-line 'next-line
    ;;                            slime-xref-mode-map)
    ;; (substitute-key-definition 'slime-xref-prev-line 'previous-line
    ;;                            slime-xref-mode-map)
    ;; (substitute-key-definition 'slime-goto-xref 'slime-show-xref
    ;;                            slime-xref-mode-map)

    (defun slime-disassemble-full-definition ()
      "Disassemble definition at point"
      (interactive)
      (slime-eval-describe
       `(swank::with-buffer-syntax
         ()
         (cl:with-output-to-string
          (cl:*standard-output*)
          (cl:let ((cl:*print-readably* cl:nil))
                  (sb-disassem:disassemble-code-component
                   (cl:eval (cl:read-from-string ,(slime-definition-at-point t)))))))))

  (load-slime)))
