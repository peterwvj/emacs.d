;;
;; Browser configuration
;;

;;
;; External browser configuration (use Google Chrome)
;;
(require 'browse-url)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;;
;; Launch google searches from within Emacs
;;
(use-package google-this
  :config
  (google-this-mode 1))


(provide 'browser-config-pvj)
