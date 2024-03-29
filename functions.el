(require 'cl)

(defun my-add-to-path (dir)
  "Add directory `dir' in +my-emacs-config-dir+ to `load-path'."
  (let ((name
         (etypecase dir
           (symbol (format "%s/%s" +my-emacs-config-dir+ dir))
           (string dir))))
    (when (file-exists-p name)
      (add-to-list 'load-path name))))

(defun my-check-or-install-package (pkg)
  (or (package-installed-p pkg)
      (package-install pkg)))

(defmacro* my-require-and-eval ((feature &optional add-to-path package-name) &body body)
  "Execute code if feature was loaded successfully.
Optinally add directory `add-to-path' to `load-path'."
  `(progn
     ,(when package-name
        `(my-check-or-install-package ',package-name))
     ,(when add-to-path
        `(my-add-to-path ',add-to-path))
     (if (require ',feature nil t)
         (progn ,@body)
       (message "my-require-and-eval: require failed for '%s'" ',feature))))

(defun my-load-init (modules)
  "Load initialization files."
  (dolist (x modules)
    (load (format "%s/init_%s" +my-emacs-config-dir+ x) t)))

(defun my-get-ip (&optional iface)
  "Get ip of the interface"
  (format-network-address
   (if iface
       (car (network-interface-info iface))
     (cdr (cl-find-if-not #'(lambda (x) (or (seq-contains '("lo" "docker0") (car x)) (> (length (cdr x)) 5))) (network-interface-list))))
   t))

(defun surround-region-and-indent (start end start-text end-text)
  (save-excursion
   (let ((rgn-txt (delete-and-extract-region start end))
         new-end)
     (save-excursion
      (goto-char start)
      (when (/= start (line-beginning-position))
        (insert "\n"))
      (insert start-text)
      (insert rgn-txt)
      (unless (= (point) (line-beginning-position))
        (insert "\n"))
      (insert end-text)
      (setq new-end (line-end-position)))
     (beginning-of-line)
     (indent-region start new-end))))

(defun avx3-comment-region (start end)
  (interactive "r")
  (surround-region-and-indent start end "#ifdef HAVE_AVX3\n" "#endif  // HAVE_AVX3\n"))

(defun oncv10-comment-region (start end)
  (interactive "r")
  (surround-region-and-indent start end "#if ONC_VERSION == 10\n" "#endif  // ONC_VERSION == 10\n"))
