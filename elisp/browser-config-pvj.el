;;
;; Browser configuration
;;

;;
;; External browser configuration (use Google Chrome)
;;
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;
;; Launch google searches from within Emacs
;;
(use-package google-this
  :config
  (google-this-mode 1))


(provide 'browser-config-pvj)
