;;
;; Scratch buffer configuration
;;

(setq initial-scratch-message "")

;;
;; Re-spawn scratch buffer when killed
;;
(use-package immortal-scratch
  :config
  (add-hook 'after-init-hook 'immortal-scratch-mode))

(provide 'scratch-config-pvj)
