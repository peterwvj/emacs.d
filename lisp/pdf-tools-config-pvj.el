;;
;; PDF tools configuration
;;

;;
;; For viewing PDF files
;;
(use-package pdf-tools
  :if (and (eq system-type 'gnu/linux) (null noninteractive))
  :config
  ;; Install pdf-tools if necessary (without asking)
  (pdf-tools-install t))

(provide 'pdf-tools-config-pvj)
