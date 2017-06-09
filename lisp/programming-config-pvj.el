
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
  :config
  (use-package helm-c-yasnippet
    :config
    (setq helm-yas-space-match-any-greedy t)
    (global-set-key (kbd "C-c y") 'helm-yas-complete))
  
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

;;
;; "Jump to definition" feature
;;
(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'helm)
  (dumb-jump-mode 1))

(provide 'programming-config-pvj)
