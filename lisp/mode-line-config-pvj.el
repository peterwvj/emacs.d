;;
;; Mode-line configuration
;;

(use-package spaceline
  :demand t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(provide 'mode-line-config-pvj)
