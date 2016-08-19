(my-add-to-path (format "%s/el/" +my-emacs-config-dir+))

(set-language-environment "UTF-8")

(setq
 default-input-method "russian-computer"
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
 fringe-mode '(1 . 1)
 global-font-lock-mode t
 ispell-program-name "aspell"
 ispell-dictionary "english"
 size-indication-mode t
 transient-mark-mode t
 bookmark-save-flag 1
 enable-local-variables :safe
 kill-read-only-ok t
 use-dialog-box nil
 history-length 500
 history-delete-duplicates t
 require-final-newline t
 diff-switches "-u"
 vc-follow-symlinks t
 dired-isearch-filenames t
 dired-bind-jump nil
 x-select-enable-primary t
 ido-use-virtual-buffers t
 ido-enable-flex-matching t
 ido-default-buffer-method 'selected-window
 recentf-save-file "~/.config/emacs/recentf"
 calendar-week-start-day 1
 kill-do-not-save-duplicates t
 scroll-conservatively 1
 confirm-kill-emacs 'y-or-n-p
 mouse-wheel-scroll-amount '(1 ((control) . 5))
 sentence-end-double-space nil)

(put 'narrow-to-region 'disabled nil)

(setq-default indent-tabs-mode nil)
(setq-default fill-column 70)
(setq-default indicate-empty-lines t)
(setq-default delete-trailing-whitespace t)

(blink-cursor-mode 0)
(ido-mode t)
(winner-mode 1)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(my-require-and-eval (evil nil evil)
   (evil-mode 1))

;; (my-require-and-eval (auto-complete-config)
;;   (ac-config-default))

(my-require-and-eval (smex nil smex)
 (global-set-key (kbd "M-x") 'smex)
 (global-set-key (kbd "M-X") 'smex-major-mode-commands))

;;; Write backup in ~/.saves
(setq
 backup-by-copying t                    ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/saves"))          ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t                      ; use versioned backups
 tramp-backup-directory-alist backup-directory-alist)

;;; Sessions mode
(my-require-and-eval (session nil session)
  (setq session-save-file
        (expand-file-name (concat (format "%s/session/" +my-emacs-config-dir+)
                                  emacs-instance)))

  (add-hook 'after-init-hook 'session-initialize))

;;; Buffers
(global-set-key "\C-x\C-b" 'ibuffer)

(my-require-and-eval (uniquify)
  (setq
   uniquify-buffer-name-style 'reverse
   uniquify-separator "/"
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*"))

(my-require-and-eval (dired-x)
  (add-hook 'dired-mode-hook
            (lambda () (dired-omit-mode 1)))
  (setq dired-omit-files "^\\..*"
        dired-omit-verbose nil))

(my-require-and-eval (company nil company)
  (eval-after-load "company"
    '(progn
       (add-to-list 'company-begin-commands 'backward-delete-char)
       (setq
        company-dabbrev-ignore-case t
        company-dabbrev-code-ignore-case t
        company-dabbrev-downcase nil
        company-minimum-prefix-length 2
        ;;something universally applied
        company-backends '((company-capf
                            :with
                            company-dabbrev-code
                            company-keywords)
                           company-dabbrev))

       ;; using it instead of `company-complete-common'
       (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
       ;; getting the normal 'C-w' behaviour back
       (define-key company-active-map (kbd "C-w") nil)
       ;; not turning it on until the first usage (is it ok?)
       (global-company-mode))))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key "\M-," 'pop-tag-mark)

;;; Remove unneeded and often accidently pressed bindings
(defun undefine-keys (&rest args)
  (dolist (x args)
    (global-set-key x nil)))

(undefine-keys
 [f1] [f2] [f3] [f4] [f10]
 [insert] [M-drag-mouse-1]
 "\M-`" "\C-\M-w" "\M-*"
 "\C-x\C-p" "\C-xm" "\M-o")

;; (setq browse-url-browser-function nil)
;; (let ((acons (assoc "." browse-url-browser-function))
;;       (browser 'browse-url-default-browser))
;;   (if acons
;;       (setf (cdr acons) browser)
;;       (setf browse-url-browser-function
;;             (list (cons "." browser)))))

;; (defun browse-url-browser (url &optional new-window)
;;   (interactive (browse-url-interactive-arg "URL: "))
;;   (start-process "browser" nil "browser" url))

(global-hi-lock-mode 1)

(my-require-and-eval (xah-replace-pairs nil xah-replace-pairs))
(my-require-and-eval (magit nil magit))
