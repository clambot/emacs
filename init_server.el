(my-require-and-eval (server)
   (setq server-use-tcp t
         server-host (my-get-ip)
         server-socket-dir (format "~/%s/server" +my-emacs-config-dir+))

   (unless (server-running-p)
     (server-start)))
