;;
;; File explorer configuration
;;

;;
;; diredfl configuration
;;
(use-package diredfl
  :config
  (diredfl-global-mode)
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t))

;;
;; Tree-based file explorer
;;
(use-package neotree
  :bind (([f7] . neotree-toggle))
  :config
  (setq neo-theme 'classic)
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  (setq neo-window-width 50)
  (setq neo-vc-integration (quote (face)))
  (setq neo-cwd-line-style 'text))

(provide 'file-explorer-config-pvj)
