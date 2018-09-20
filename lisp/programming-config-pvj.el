
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

(defun pvj/bury-compile-buffer-if-successful (buffer string)
  "Bury compilation buffer if compilation succeeded without warnings.
Argument BUFFER the buffer to be buried.
Argument STRING the compilation output."
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (delete-windows-on buf))
                    buffer)))
(add-hook 'compilation-finish-functions 'pvj/bury-compile-buffer-if-successful)

;; stop scrolling at first error
(setq compilation-scroll-output 'first-error)

;;
;; Support for Python
;;
(use-package elpy
  :bind
  (:map inferior-python-mode-map
        ("C-c l" . comint-clear-buffer))
  :config
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
;; Company mode backend for C/C++ header files
;;
(use-package company-c-headers
  :config
  (add-to-list 'company-backends 'company-c-headers))

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
            (smartparens-mode -1)
            (setq-local global-hl-line-mode nil)))

;; Make the up and down arrow keys behave as in a shell
(add-hook 'inferior-octave-mode-hook
          (lambda ()
            (define-key inferior-octave-mode-map [up]
              'comint-previous-input)
            (define-key inferior-octave-mode-map [down]
              'comint-next-input))) 

(use-package package-lint)

(use-package flycheck-package
  :config
  (eval-after-load 'flycheck
    '(flycheck-package-setup)))

(use-package flycheck-vdm)
(use-package vdm-snippets)
(use-package vdm-mode
  :config
  (setq flycheck-vdm-tool-jar-path "/home/peter/dev/vdmj/vdmj-4.1.0-P-180112.jar")
  (vdm-mode-setup))



(provide 'programming-config-pvj)
