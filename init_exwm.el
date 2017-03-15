;;; exwm
;;(add-to-list 'load-path "/usr/share/emacs/xelb/")
;;(add-to-list 'load-path "/usr/share/emacs/exwm/")

;; (setq exwm-debug-on t)
(require 'exwm)
;; Fix problems with Ido
(require 'exwm-config)
(exwm-config-ido)
;; (exwm-config-default)


(exwm-input-set-key (kbd "<s-f5>") #'exwm-reset)
(exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)
(exwm-input-set-key (kbd "s-k") #'exwm-input-toggle-keyboard)
(exwm-input-set-key (kbd "s-<right>") #'windmove-right)
(exwm-input-set-key (kbd "s-<left>") #'windmove-left)
(exwm-input-set-key (kbd "s-<up>") #'windmove-up)
(exwm-input-set-key (kbd "s-<down>") #'windmove-down)

(setq exwm-workspace-number 10)

(exwm-input-set-key (kbd "s-0")
                    (lambda () (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-1")
                    (lambda () (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-2")
                    (lambda () (interactive) (exwm-workspace-switch 2)))
(exwm-input-set-key (kbd "s-3")
                    (lambda () (interactive) (exwm-workspace-switch 3)))
(exwm-input-set-key (kbd "s-4")
                    (lambda () (interactive) (exwm-workspace-switch 4)))
(exwm-input-set-key (kbd "s-5")
                    (lambda () (interactive) (exwm-workspace-switch 5)))
(exwm-input-set-key (kbd "s-6")
                    (lambda () (interactive) (exwm-workspace-switch 6)))
(exwm-input-set-key (kbd "s-7")
                    (lambda () (interactive) (exwm-workspace-switch 7)))
(exwm-input-set-key (kbd "s-8")
                    (lambda () (interactive) (exwm-workspace-switch 8)))
(exwm-input-set-key (kbd "s-9")
                    (lambda () (interactive) (exwm-workspace-switch 9)))

(exwm-input-set-key (kbd "s-r")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))


;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
;; when a new window class name or title is available. Here's some advice on
;; this subject:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + Only renaming buffer in one hook and avoid it in the other. There's no
;;   guarantee on the order in which they are run.
;; + For applications with multiple windows (e.g. GIMP), the class names of all
;;   windows are probably the same. Using window titles for them makes more
;;   sense.
;; + Some application change its title frequently (e.g. browser, terminal).
;;   Its class name may be more suitable for such case.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(setq exwm-workspace-number 10)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; (require 'exwm-randr)
;; (setq exwm-randr-workspace-output-plist '(1 "HDMI1" 2 "HDMI1" 3 "LVDS1" 4 "LVDS1" 5 "HDMI1"))
;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil "xrandr --output HDMI1 --primary --mode 1920x1080 --pos 1440x0 --output LVDS1 --mode 1440x900 --pos 0x180")))
;; (exwm-randr-enable)

;; (exwm-input-set-key (kbd "s-<Scroll_Lock>")
;;                     (lambda () (interactive) (start-process "" nil "xlock")))

;; You can hide the mode-line of floating X windows by uncommenting the
;; following lines
(add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook #'exwm-layout-show-mode-line)

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line
;(setq exwm-workspace-minibuffer-position 'bottom)

(exwm-enable)
