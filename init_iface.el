(setq inhibit-startup-message t
      font-lock-maximum-decoration '((lisp-mode . t)
                                     (emacs-lisp-mode . t)
                                     (t . t)))

;; Disable menu-bar, tool-bar and scroll-bar to increase the usable space
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; You may want Emacs to show you the time
(setq display-time-default-load-average nil)
(display-time-mode t)

(setq default-frame-alist
      (append
       (list
        '(width . 80)
        '(height . 60)
        '(font . "-misc-fixed-medium-r-normal--15-*-*-*-*-*-iso10646-1"))
       default-frame-alist))

(setq initial-frame-alist default-frame-alist)
(setq special-display-frame-alist default-frame-alist)

(setq default-tab-width 4)
(setq tab-width 4)
(setq c-basic-offset 4)

(my-check-or-install-package 'tango-2-theme)
(load-theme 'tango-2 t)
