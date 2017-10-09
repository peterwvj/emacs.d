;;
;; Mode-line configuration
;;

(use-package spaceline
  :demand t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode))

(use-package winum
  :config
  (setq winum-auto-assign-0-to-minibuffer nil
        winum-auto-setup-mode-line nil
        winum-ignored-buffers '(" *which-key*"))
  (winum-mode))

(provide 'mode-line-config-pvj)
