;; (require 'w3m)
;; ;; (if (null w3m-command-arguments) (setq w3m-command-arguments ""))
;; (setq w3m-command-arguments
;;       (nconc w3m-command-arguments
;;              '("-o" "http_proxy=http://proxy.com/")
;;              '("-o" "https_proxy=http://proxy.com/")))

;; (setq w3m-no-proxy-domains '("local"))
