;;
;; PDF tools configuration
;;

;;
;; For viewing PDF files
;;
(use-package pdf-tools
  :if (and (eq system-type 'gnu/linux) (null noninteractive))
  :config
  (pdf-tools-install))

(provide 'pdf-tools-config-pvj)
