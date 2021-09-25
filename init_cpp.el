(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq c-basic-offset 4)
(c-set-offset 'innamespace 0)
(c-set-offset 'arglist-close 0)
(c-set-offset 'arglist-intro '+)
(c-set-offset 'substatement-open 0)
(c-set-offset 'statement-cont '+)
(c-set-offset 'arglist-cont-nonempty '+)

(my-require-and-eval (ggtags)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode)
                (modify-syntax-entry ?_ "w")
                (ggtags-mode 1)))))

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(eval-after-load 'ggtags
  '(progn
     (evil-make-overriding-map ggtags-mode-map 'normal)
     ;; force update evil keymaps after ggtags-mode loaded
     (add-hook 'ggtags-mode-hook #'evil-normalize-keymaps)))

;; (my-require-and-eval
;;  (ac-clang nil ac-clang)
;;  (ac-clang-initialize))

(my-require-and-eval
 (fill-column-indicator nil fill-column-indicator)
 (setq-default fci-rule-column 80)
 (setq fci-rule-width 1)
 (setq fci-rule-color "darkblue"))

;;; fix ac vs fci-mode
(defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
             (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

;;;
;;; CEDET
;;;

(my-require-and-eval (cedet))

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-show-parser-state-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(setq semantic-idle-scheduler-idle-time 10)
(setq semanticdb-default-save-directory (format "%s/semanticdb" +my-emacs-config-dir+))

(semantic-mode t)

(require 'semantic/ia)
(require 'semantic/bovine/gcc)

(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS")
  (setq semanticdb-find-default-throttle '(file local project unloaded system recursive)))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

;; (global-ede-mode t)
;; (require 'ede/generic)
;; (ede-enable-generic-projects)

(defun my-c-mode-hook ()
  (google-set-c-style)
  (fci-mode 1)
  (display-line-numbers-mode t)
  (setq auto-hscroll-mode 'current-line)
  (add-to-list 'ac-sources 'ac-source-yasnippet)
  (add-to-list 'ac-sources 'ac-source-semantic)
  (add-to-list 'ac-sources 'ac-source-gtags))
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;; (load-file (format "%s/ede_projects.el" +my-emacs-config-dir+))

(my-require-and-eval (yasnippet nil yasnippet)
  (eval-after-load "yasnippet"
    '(progn
      (setq yas-snippet-dirs (list (expand-file-name "snippets" +my-emacs-config-dir+)))
       (yas-reload-all)))
  (yas-global-mode 1))
