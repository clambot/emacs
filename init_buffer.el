(defun my-move-buffer (buf to)
  (let ((other-win (windmove-find-other-window to))
        (this-buffer (window-buffer (selected-window))))
    (if (or (null other-win)
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "Can't move buffer to %s" to))
    (previous-buffer)
    (set-window-buffer other-win this-buffer)))

(defun my-move-buffer-left ()
  (interactive)
  (my-move-buffer (current-buffer) 'left))

(defun my-move-buffer-right ()
  (interactive)
  (my-move-buffer (current-buffer) 'right))

(defun my-move-buffer-up ()
  (interactive)
  (my-move-buffer (current-buffer) 'up))

(defun my-move-buffer-down ()
  (interactive)
  (my-move-buffer (current-buffer) 'down))

(global-set-key (kbd "C-x <M-right>") 'my-move-buffer-right)
(global-set-key (kbd "C-x <M-left>") 'my-move-buffer-left)
(global-set-key (kbd "C-x <M-up>") 'my-move-buffer-up)
(global-set-key (kbd "C-x <M-down>") 'my-move-buffer-down)

;; (windmove-default-keybindings)
