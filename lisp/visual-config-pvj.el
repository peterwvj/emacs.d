
;;
;; Configuration related to visualisation and highlighting
;;

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
(use-package auto-highlight-symbol
  ;; :init (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode)
  :config
  (setq ahs-idle-interval 1.0)
  (setq ahs-default-range 'ahs-range-whole-buffer))

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
;; Show tildes in the fringe on empty lines
;;
(use-package vim-empty-lines-mode
  :init
  (add-hook 'prog-mode-hook 'vim-empty-lines-mode)
  (add-hook 'text-mode-hook 'vim-empty-lines-mode))

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
;; Show line numbers
;;
(let ((emacs26 "26.1"))
  (if (version= emacs-version emacs26)
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
