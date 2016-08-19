;; (package-initialize)

(defconst +my-emacs-config-dir+ "~/.emacs.d")

(setq custom-file "~/.emacs.d/init_custom.el")

(setq emacs-instance "general")

(load (format "%s/functions.el" +my-emacs-config-dir+))

(my-load-init '(package iface server main auto_complete misc lang tramp custom eshell))

;; (my-load-init '(exwm))
