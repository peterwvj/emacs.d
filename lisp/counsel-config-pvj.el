;;
;; Counsel configuration
;;


(use-package ivy
  :bind
  (("C-c r" . ivy-resume))
  ;; :diminish (ivy-mode . "")
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package swiper
  :bind
  (("C-c s" . swiper)))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-h v" . counsel-describe-variable)
   ("C-h f" . counsel-describe-function)
   ("C-c C-m" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-x m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c a" . counsel-ag))
  :config
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)")))

;; Use smex to give counsel M-x command history
(use-package smex)

(use-package counsel-projectile
  :config
  (setq counsel-projectile-switch-project-action 'dired)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode))

(use-package flyspell-correct-ivy
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  :config
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper))

(provide 'counsel-config-pvj)
