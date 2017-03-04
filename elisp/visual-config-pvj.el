
;;
;; Configuration related to visualisation and highlighting
;;

;; Display the current column
(setq column-number-mode t)

;;
;;  Show matching parentheses
;;
(show-paren-mode 1)
(setq show-paren-delay 0)

;;
;; Show line numbers
;;
(use-package nlinum
  :pin "gnu"
  :config
  (setq nlinum-format " %d ")
  (global-nlinum-mode))

;;
;; Highlights delimiters such as parentheses, brackets or braces according to their depth
;;
(use-package rainbow-delimiters
  :config
  ;; To start the mode automatically in most programming modes (Emacs 24 and above):
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;
;; No blinking cursor
;;
(blink-cursor-mode 0)

;; Use the naquadah theme. In addition, the cursor color has been
;; changed to green, and the text-selection color (the region face)
;; has been changed to blue
(use-package naquadah-theme
  :config
  (load-theme 'naquadah t))

;;
;; Hide Emacs toolbar
;;
(tool-bar-mode -1)

;;
;; Hide menu-bar
;;
(menu-bar-mode -1)

;;
;; Hide scroll-bar
;;
(when (null noninteractive)
  (scroll-bar-mode -1))

;;
;; Alarm bell config
;;
(setq ring-bell-function 'ignore) ;; Completely turn off the alarm bell

;; Disable hl-line-mode for ansi-term
(add-hook 'term-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

;; Do not show line numbers
(add-hook 'term-mode-hook (lambda ()
                            (nlinum-mode -1)))

;;
;; Highlight the current line
;;
(use-package color)

;; Sets the hl-line face to have no foreground and a background
;; that is 10% darker than the default face's background.
(defun set-hl-line-color-based-on-theme ()
  "Highlight line based on theme."
  (set-face-attribute 'hl-line nil
                      :foreground nil
                      :background (color-darken-name (face-background 'default) 10)))

(add-hook 'global-hl-line-mode-hook 'set-hl-line-color-based-on-theme)

(global-hl-line-mode 1)

;; Do not highlight current line in help mode
(add-hook 'help-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

(use-package auto-highlight-symbol
  :init (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode)
  :config
  (setq ahs-idle-interval 1.0)
  (setq ahs-default-range 'ahs-range-whole-buffer) ;; highlight every occurence in buffer
  :bind (:map auto-highlight-symbol-mode-map
              ("M-p" . ahs-backward)
              ("M-n" . ahs-forward)))

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
  (add-hook 'prog-mode-hook 'vim-empty-lines-mode))

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

(provide 'visual-config-pvj)
