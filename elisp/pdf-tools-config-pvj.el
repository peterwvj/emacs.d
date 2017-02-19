;;
;; PDF tools configuration
;;

;;
;; For viewing PDF files
;;
(use-package pdf-tools
  :if (and (string-equal system-type "gnu/linux") (null noninteractive))
  :config
  (pdf-tools-install))

(provide 'pdf-tools-config-pvj)
