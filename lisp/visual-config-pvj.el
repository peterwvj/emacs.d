
;;
;; Configuration related to visualisation and highlighting
;;

(setq frame-inhibit-implied-resize t)

(use-package naquadah-theme
  :config
  (load-theme 'naquadah t))

(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Display the current column
(setq column-number-mode t)

;;
;;  Show matching parentheses
;;
(show-paren-mode 1)
(setq show-paren-delay 0)

;;
;; Highlights delimiters such as parentheses, brackets or braces according to their depth
;;
(use-package rainbow-delimiters
  :config
  ;; To start the mode automatically in most programming modes (Emacs 24 and above):
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package beacon
  :config
  (setq-default beacon-lighter "")
  (setq-default beacon-size 5)
  (add-hook 'after-init-hook 'beacon-mode))

;;
;; Blink cursor
;;
(blink-cursor-mode 1)

;;
;; Visual mode-line bell
;;
(use-package mode-line-bell
  :config
  (mode-line-bell-mode))

;; Disable hl-line-mode for ansi-term
(add-hook 'term-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

;; Do not highlight current line in help mode
(add-hook 'help-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

;; highlight every occurence in buffer
(use-package symbol-overlay
  :custom-face
  (symbol-overlay-default-face ((t (:inherit 'region))))
  (symbol-overlay-face-1 ((t (:inherit 'highlight))))
  (symbol-overlay-face-2 ((t (:inherit 'font-lock-builtin-face :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit 'warning :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit 'font-lock-constant-face :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit 'error :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit 'dired-mark :inverse-video t :bold nil))))
  (symbol-overlay-face-7 ((t (:inherit 'success :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit 'dired-symlink :inverse-video t :bold nil))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all))
  :hook ((prog-mode . symbol-overlay-mode)))


(use-package highlight-numbers
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;;
;; Highlight TODOs
;;
(use-package hl-todo
  :init (global-hl-todo-mode)
  :config
  (setq hl-todo-activate-in-modes '(prog-mode)))

;;
;; Display current match and total matches in the mode-line in various search modes
;;
(use-package anzu
  :config
  (global-anzu-mode +1)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

;;
;; Enable word wrapping
;;
(global-visual-line-mode 1)

;;
;; Highlight current line
;;
(global-set-key (kbd "C-c l") 'hl-line-mode)

;;
;; Show line numbers
;;
(let ((emacs26 "26.1"))
  (if (version<= emacs26 emacs-version)
      (global-set-key (kbd "C-c n") 'display-line-numbers-mode)
    (use-package nlinum
      :pin "gnu"
      :bind
      (("C-c n" . nlinum-mode))
      :config
      (setq nlinum-format " %d ")
      (setq nlinum-highlight-current-line t))))

(require 'time)
;; Display time format
(setq display-time-day-and-date t
      display-time-default-load-average nil
      display-time-24hr-format t)

(provide 'visual-config-pvj)
