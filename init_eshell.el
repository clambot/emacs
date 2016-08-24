(defun my-eshell (n)
  (interactive "xn: ")
  (eshell n))

(defun fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
           (setq str (concat str
                             (cond ((= 0 (length (car components))) "/")
                                   ((= 1 (length (car components)))
                                    (concat (car components) "/"))
                                   (t
                                    (if (string= "."
                                                 (string (elt (car components) 0)))
                                        (concat (substring (car components) 0 2)
                                                "/")
                                        (string (elt (car components) 0) ?/)))))
                 len (- len (1- (length (car components))))
                 components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (fish-path (eshell/pwd) 20)
         (if (= (user-uid) 0) " $ " " # "))))

(my-require-and-eval (em-alias)
 (eshell/alias "e" "find-file $1")
 (eshell/alias "d" "dired $1")
 (eshell/alias "ll" "ls -al $1")
 (eshell/alias "lr" "ls -lrt $1")
 (eshell/alias "new" "(eshell 'z)"))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (setq eshell-path-env (concat "/tools/smi/apps/bin:" eshell-path-env))
              (setenv "PATH" (concat "/tools/smi/apps/bin:" (getenv "PATH")))))
