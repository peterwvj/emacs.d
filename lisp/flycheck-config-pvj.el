;;
;; Flycheck configuration
;;

;; Syntax checking
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-emacs-lisp-load-path 'inherit))


;; Shows errors using pos-tip popups
(use-package flycheck-pos-tip
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

(provide 'flycheck-config-pvj)
