(my-check-or-install-package 'auto-complete)

(my-require-and-eval (ac-c-headers nil ac-c-headers)
  (add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))
