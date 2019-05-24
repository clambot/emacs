(require 'widget)

(defvar x-val nil)
(defvar y-val nil)
(defvar z-val nil)

(defun my/add-var (name)
  (let ((wid-val (widget-create 'editable-field
                                :size 30
                                ;; :valid-regexp "[0-9a-fA-Fx]"
                                :format (format "%s: %%v" name)
                                :action (lambda (wid &rest ignore)
                                          (let ((val (widget-value wid)))
                                            (loop for i from 0 below 64
                                                  for b = (calc-eval '("and($, lsh(1, $$, 64), 64)") 'num val i)
                                                  with bits = (widget-get wid :bits)
                                                  do (widget-value-set (nth i bits)
                                                                       (if (equal b "0") nil t)))
                                            (loop for i from 0 below 16
                                                  for pos = (* i  4)
                                                  for n = (calc-eval '("and(rsh($, $$, 64), 16#f, 8)") 'num val pos)
                                                  with nibbles = (widget-get wid :nibbles)
                                                  do (widget-value-set (nth i nibbles)
                                                                       (format "% 4X" (string-to-int n))))))
                                "16#0")))
    (widget-insert "\n")
    (widget-insert "63 60|59  56|55 52|51 48|47  44|43 40|39 36|35 32|31  28|27 24|23  20|19 16|15  12|11  8|7   4|3   0\n")
    (widget-put wid-val
                :bits
                (loop for i from 1 to 64
                      for w = (widget-create 'checkbox
                                             :notify (lambda (wid &rest ignore)
                                                       (let* ((wid-val (widget-get wid :wid-val))
                                                              (pos (- 64 (widget-get wid :pos)))
                                                              (bit (if (widget-value wid) 1 0))
                                                              (val-a (widget-value wid-val)))
                                                         ;; (message "pos:%d, bit:%d, val:%s" pos bit val-a)
                                                         (save-excursion
                                                          (widget-value-set
                                                            wid-val
                                                            (format "%s" (calc-eval '("or(diff($, lsh(1, $$, 64), 64), lsh($$$, $$, 64), 64)" calc-number-radix 16) 'num val-a pos bit))
                                                            ))))
                                             nil)
                      collecting w into bits
                      do (progn (widget-put w :pos i)
                                (widget-put w :wid-val wid-val))
                      when (and (/= i 64) (= (logand i 3) 0))
                      do (widget-insert "|")
                      finally (return (reverse bits))))
    (widget-insert "\n")
    (widget-put wid-val
                :nibbles
                (loop for i from 1 to 16
                      for w = (widget-create 'push-button
                                             ;; :button-face 'custom-button
                                             (format "% 4d" 0))
                      collecting w into nibbles
                      when (and (= (% i 3) 0))
                      do (widget-insert " ")
                      finally (return (reverse nibbles))))
    wid-val))

(defvar my/ed-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    ;; (define-key map (kbd "s-`") 'delete-window)
    map)
  "")

(defun my/ed (buffer-name &optional bits)
  (interactive)
  (if (get-buffer buffer-name)
      (switch-to-buffer buffer-name)
      (progn
        (switch-to-buffer buffer-name)
        (make-local-variable 'x-val)
        (make-local-variable 'y-val)
        (make-local-variable 'z-val)
        (kill-all-local-variables)
        (let ((inhibit-read-only t))
          (erase-buffer))
        (remove-overlays)
        (setq x-val (my/add-var "x"))
        (widget-insert "\n")
        (setq y-val (my/add-var "y"))
        (widget-insert "\n")
        (setq z-val (my/add-var "z"))
        (widget-insert "\n")
        (use-local-map my/ed-mode-map)
        (widget-setup))))

(global-set-key (kbd "s-`") (lambda ()
                              (interactive)
                              (let ((win (get-buffer-window "my/ed" 'visible)))
                                (if win
                                    (if (equal win (selected-window))
                                        (delete-window win)
                                        (select-window win))
                                    (progn
                                      (select-window (split-window (frame-root-window) -15 'above))
                                      (my/ed "my/ed"))))))
