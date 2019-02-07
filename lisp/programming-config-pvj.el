
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
;; Compilation options
;;

;; stop scrolling at first error
(setq compilation-scroll-output 'first-error)

;; comint configuration
(setq comint-prompt-read-only t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
;;
;; Support for Python
;;
(use-package elpy
  :bind
  (:map inferior-python-mode-map
        ("C-c l" . comint-clear-buffer))
  :config
  (setq elpy-rpc-python-command "python3")
  (setq python-shell-interpreter "python3")
  (setq elpy-rpc-backend "jedi")
  (elpy-enable))

;;
;; Groovy
;;
(use-package groovy-mode
  :init
  (add-to-list 'auto-mode-alist '("Jenkinsfile" . groovy-mode)))

;;
;; Lua
;;
(use-package lua-mode)

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
  :bind
  (("C-c y" . yas-insert-snippet))
  :config
  (setq yas-verbosity 2)
  (yas-global-mode 1)
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  ;; Alternatively use Control-c + tab
  (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand))

;; Official Yasnippet collections
(use-package yasnippet-snippets
  :after (yasnippet))

;;
;; XML configuration
;;
(setq nxml-slash-auto-complete-flag t)

;;
;; "Jump to definition" feature
;;
(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'helm)
  (dumb-jump-mode 1))

;;
;; C configuration
;;
(setq c-default-style "linux"
      c-basic-offset 4)

;;
;; Increment/decrement numbers at point
;;
(use-package evil-numbers)

;;
;; web-mode
;;
(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.vm\\'" . web-mode)
         )
  :config
  (setq web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-expanding t
        web-mode-enable-auto-indentation t))

;;
;; Octave mode
;;
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (setq-local global-hl-line-mode nil)))

;; Make the up and down arrow keys behave as in a shell
(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input))) 

(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package package-lint)

(use-package flycheck-package
  :config
  (eval-after-load 'flycheck
    '(flycheck-package-setup)))

(use-package vdm-comint
  :config
  ;; Inconvenient to treat ` as a pair in vdm-mode and vdm-comint-mode
  (eval-after-load 'smartparens
    '(sp-local-pair #'vdm-mode "`" nil :actions nil)))

(use-package flycheck-vdm)
(use-package vdm-snippets)
(use-package vdm-mode
  :config
  (setq flycheck-vdm-tool-jar-path "/home/peter/dev/vdmj/vdmj.jar")
  (eval-after-load 'smartparens
    '(sp-local-pair #'vdm-comint-mode "`" nil :actions nil))
  (vdm-mode-setup))

(use-package imenu-list)

(use-package lsp-java
  :hook (java-mode . (lambda ()
                       (setq c-basic-offset 4
                             tab-width 4
                             indent-tabs-mode t
                             imenu-list-auto-resize t)
                       (imenu-list-minor-mode)
                       (lsp))))

(provide 'programming-config-pvj)
