;; (package-initialize)

(defconst +my-emacs-config-dir+ "~/.emacs.d")

(setq custom-file "~/.emacs.d/init_custom.el")

(setq emacs-instance "general")

(load (format "%s/functions.el" +my-emacs-config-dir+))

(my-load-init '(proxy package iface server main auto_complete misc lang
                tramp custom eshell ediff buffer w3m))

(my-load-init '(exwm))
;; (my-load-init '(my-ed))
(put 'narrow-to-page 'disabled nil)
(put 'list-threads 'disabled nil)
