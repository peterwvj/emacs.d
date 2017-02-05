;;
;; diminish configuration
;;

(use-package diminish
  :config
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode))

  (eval-after-load "projectile"
    '(diminish 'projectile-mode))

  (eval-after-load "helm-mode"
    '(diminish 'helm-mode))

  ;; Use '✈' to show that flyspell is enabled
  (eval-after-load "flyspell"
    '(diminish 'flyspell-mode (string 32 #x2708)))

  (eval-after-load "guess-language"
    '(diminish 'guess-language-mode "GL"))
  
  (add-hook 'lisp-mode-hook
            (lambda()
              (setq mode-name "λ"))) 

  (add-hook 'emacs-lisp-mode-hook 
            (lambda()
              (setq mode-name "ξλ"))) 

  (eval-after-load "reftex"
    '(diminish 'reftex-mode))

  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode))

  (eval-after-load "hungry-delete"
    '(diminish 'hungry-delete-mode))

  (eval-after-load "volatile-highlights"
    '(diminish 'volatile-highlights-mode))

  (eval-after-load "google-this"
    '(diminish 'google-this-mode))

  (eval-after-load "auto-highlight-symbol"
    '(diminish 'auto-highlight-symbol-mode))

  (eval-after-load "which-key"
    '(diminish 'which-key-mode))

  (eval-after-load "page-break-lines"
    '(diminish 'page-break-lines-mode))

  (eval-after-load "vim-empty-lines-mode"
    '(diminish 'vim-empty-lines-mode))

  (diminish 'overwrite-mode)
  
  (diminish 'visual-line-mode)

  (diminish 'outline-minor-mode))

(provide 'diminish-config-pvj)
