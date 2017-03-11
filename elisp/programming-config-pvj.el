
;;
;; Programming related configuration
;;

;;
;; Compile Elisp sources automatically
;;
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;
;; Support for Python
;;
(use-package elpy
  :config
  (elpy-enable))

;;
;; Groovy
;;
(use-package groovy-mode
  :init
  (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode)))

;;
;; Clojure Interactive Development Environment that Rocks
;;
(use-package cider
  :bind
  (:map cider-repl-mode-map
        ("C-l" . cider-repl-clear-buffer))
  :config
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (setq cider-repl-wrap-history t)
  (setq  nrepl-hide-special-buffers t))

;;
;; Use YASnippet templates
;;
(use-package yasnippet
  :config
  (yas-global-mode 1)
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  ;; Alternatively use Control-c + tab
  (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand))

;;
;; XML configuration
;;
(setq nxml-slash-auto-complete-flag t)

(provide 'programming-config-pvj)
