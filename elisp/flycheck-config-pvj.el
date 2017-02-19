;;
;; Flycheck configuration
;;

;; Syntax checking
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(provide 'flycheck-config-pvj)
