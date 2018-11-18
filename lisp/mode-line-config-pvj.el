;;
;; Mode-line configuration
;;

(use-package spaceline
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator 'wave)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-major-mode-off)
  (spaceline-toggle-helm-help-off)
  (spaceline-toggle-helm-done-off)
  (spaceline-toggle-helm-follow-off)
  (spaceline-toggle-helm-number-off)
  (spaceline-toggle-helm-buffer-id-off)
  (spaceline-toggle-version-control-off)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-encoding-abbrev-off))

(use-package winum
  :config
  (setq winum-auto-assign-0-to-minibuffer nil
        winum-auto-setup-mode-line nil
        winum-ignored-buffers '(" *which-key*"))
  (winum-mode))

(provide 'mode-line-config-pvj)
