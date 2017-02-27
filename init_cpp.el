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

(semantic-mode t)

(require 'semantic/ia)
(require 'semantic/bovine/gcc)

(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

;; (global-ede-mode t)
;; (require 'ede/generic)
;; (ede-enable-generic-projects)

(defun my-c-mode-cedet-hook ()
  (add-to-list 'ac-sources 'ac-source-gtags)
  (add-to-list 'ac-sources 'ac-source-semantic))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)

;; (load-file (format "%s/ede_projects.el" +my-emacs-config-dir+))

(my-require-and-eval (yasnippet nil yasnippet)
  (eval-after-load "yasnippet"
    '(progn
      (setq yas-snippet-dirs (list (expand-file-name "snippets" +my-emacs-config-dir+)))
       (yas-reload-all)))
  (yas-global-mode 1))
