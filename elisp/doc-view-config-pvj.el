;;
;; DocView configuration
;;

;;
;; Disable line numbers in doc-view-mode to avoid Emacs hanging
;;
(add-hook 'doc-view-mode-hook
  (lambda ()
    (nlinum-mode -1)))

(provide 'doc-view-config-pvj)
