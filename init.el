
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;;
;; Use 'use-package'
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package)
(setq use-package-always-ensure t)
(setq use-package-always-pin "melpa")

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Set the custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Get rid of the splash screen and echo area message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;;
;; Use persistent scratch buffer
;;
(use-package persistent-scratch)
(setq initial-scratch-message "")
(persistent-scratch-setup-default)

;;
;; Remember recently opened files
;;
(setq recentf-exclude '("/Maildir/" "/.emacs.d/" ".aspell."))
(setq recentf-max-saved-items 50)

;; C-x R to view recently opened files
(use-package dired+)
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)
(setq dired-listing-switches "-alh")
(define-key dired-mode-map (kbd "M-s") 'diredp-find-a-file)

;;
;; Configure helm mode
;;
(use-package helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
;; To open as SU type C-c r (after typing C-x C-f)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-c m") 'helm-mu-contacts)

;; Mark buffers with C-SPACE and kill them with M-D
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x k") 'kill-this-buffer)


(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)
(setq helm-ff-newfile-prompt-p nil)

;; I used to have a problem where C-x C-f, i.e. helm-find-files would make emacs hang.
;; Workaround: by setting this flag I can prevent Emacs from hanging...
(setq helm-split-window-in-side-p t)

;;
;; Helm interface for Emacs 'describe-bindings
;;
(use-package helm-descbinds)
(helm-descbinds-mode)

;; helm interface for projectile
(use-package helm-projectile)
(projectile-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; helm swoop
(use-package helm-swoop)

(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
;; (setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

;;
;; Enable winner-mode
;;
(winner-mode 1)

;;
;; Enable undo-tree
;;
(use-package undo-tree)
(global-undo-tree-mode)

;;
;; Show line numbers
;;
(global-linum-mode t)

;;
;; Setup LateX
;;
(require 'latex-config-pvj)

;;
;; For viewing PDF files
;;
(use-package pdf-tools
  :if (and (string-equal system-type "gnu/linux") (null noninteractive))
  :config 
  (pdf-tools-install))


;;
;; Disable line numbers in doc-view-mode (avoid Emacs hanging)
;;
(add-hook 'doc-view-mode-hook
  (lambda ()
    (linum-mode -1)))

;;
;; Functionality that supports text editing
;;
(require 'text-config-pvj)
(global-set-key (kbd "<f8>")   'pvj/switch-language)
(global-set-key (kbd "C-q") 'pvj/duplicate-current-line-or-region)
(global-set-key (kbd "C-d") 'kill-whole-line)
(delete-selection-mode 1)
(global-set-key (kbd "C-w") 'pvj/kill-word-or-region)
(global-set-key (kbd "<f6>") 'helm-show-kill-ring)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)


;;
;; Display current match and total matches in the mode-line in various search modes
;;
(use-package anzu)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)

(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-threshold 50)
 '(anzu-replace-to-string-separator " => "))

(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)

;;
;; For language analysis
;;
(use-package langtool)
(setq langtool-language-tool-jar "~/tools/LanguageTool-3.4/languagetool-commandline.jar")

;;
;; Enable word wrapping
;;
(global-visual-line-mode 1)

;;
;; Put backup files in same directory (to avoid having emacs creating files everywhere)
;;
(setq backup-directory-alist `(("." . "~/.saves")))

;;
;;  Show matching parentheses
;;
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match "deep sky blue")
(set-face-foreground 'show-paren-match "black")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

;;
;; Highlights delimiters such as parentheses, brackets or braces according to their depth
;;
(use-package rainbow-delimiters)
;; To start the mode automatically in most programming modes (Emacs 24 and above):
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;
;; No blinking cursor
;;
(blink-cursor-mode 0)

;; Use the naquadah theme. In addition, the cursor color has been
;; changed to green, and the text-selection color (the region face)
;; has been changed to blue
(use-package naquadah-theme)
(load-theme 'naquadah t)

;; Display the current column
(setq column-number-mode t)

;;
;; Show file name of current buffer (title frame currently disabled)
;;
(setq frame-title-format
     (list (format "%s %%S: %%j " (system-name))
       '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(put 'upcase-region 'disabled nil)

;;
;; Convenient way to tell emacs 'yes' or 'no'
;;
(defalias 'yes-or-no-p 'y-or-n-p)

;; If a file or buffer does not exist when using C-x C-f or C-x b then
;; skip the confirmation
(setq confirm-nonexistent-file-or-buffer nil)

;; Kill buffers that have a live process attached - without asking!
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; Move text
(use-package move-text)
(move-text-default-bindings)

;;
;; Disable auto-copy to clipboard for mouse
;;
(setq mouse-drag-copy-region nil)

;;
;; To avoid typing ESC-ESC-ESC to escape or quit
;;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;
;; Convenient way to move between windows
;;
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)


;; Always follow symlinks
(setq vc-follow-symlinks t)

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


;; go-up for eshell
(use-package eshell-up)

;;
;; eshell configuration
;;
(require 'eshell-config-pvj)
(global-set-key (kbd "<f3>") 'eshell)
(global-set-key (kbd "<f4>") 'eshell-here)

;;
;; ansi-term config
;;

;; Disable hl-line-mode for ansi-term
(add-hook 'term-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

;; Do not show line numbers
(add-hook 'term-mode-hook (lambda ()
                            (linum-mode -1)))

;; Start ansi-term in /bin/bash/
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;;
;; Highlight the current line
;;
(use-package color)

(defun set-hl-line-color-based-on-theme ()
  "Sets the hl-line face to have no foregorund and a background
    that is 10% darker than the default face's background."
  (set-face-attribute 'hl-line nil
                      :foreground nil
                      :background (color-darken-name (face-background 'default) 10)))

(add-hook 'global-hl-line-mode-hook 'set-hl-line-color-based-on-theme)

(global-hl-line-mode 1)

;; Do not highlight current line in help mode
(add-hook 'help-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

;;
;; External browser configuration (use Google Chrome)
;;
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;
;; mu4e configuration
;;
(use-package mu4e-config-pvj
  :if (string-equal system-type "gnu/linux")
  :ensure f
  :bind (("<f2>" . mu4e))
  :config
  (progn
    ;;
    ;; imapfilter configuration
    ;; Inspired by https://www.reddit.com/r/emacs/comments/202fon/email_filters_in_mu4e/
    ;;
    (add-hook 'mu4e-update-pre-hook 'pvj/imapfilter)
    (defun pvj/imapfilter ()
      (with-current-buffer (get-buffer-create " *imapfilter*")
        (goto-char (point-max))
        (insert "---\n")
        (call-process "imapfilter" nil (current-buffer) nil "-v")))

    ;; Start mu4e
    (mu4e)

    ;; Make sure the gnutls command-line utils are installed, package
    ;; 'gnutls-bin' in Debian/Ubuntu.
    (use-package smtpmail)

    ;; Use same authinfo file for work and private emails
    (setq message-send-mail-function 'smtpmail-send-it
          starttls-use-gnutls t
          smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
          smtpmail-debug-info t)))

;;
;; Scrolling
;;
;; This seems to be the best way to achieve "smooth scrolling"
;; See - https://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-conservatively 10000)

;; Also applies to new clients that connect to the Emacs server
(add-to-list 'default-frame-alist '(fullscreen . maximized)) 

;;
;; Company mode (auto completion)
;;
(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-C-/") 'company-complete)

;;
;; Use YASnippet templates
;;
(use-package yasnippet)
(yas-global-mode 1)
;; Remove Yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Set Yasnippet's key binding to shift+tab
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; Alternatively use Control-c + tab
(define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

;;
;; org-mode
;;
(setq org-support-shift-select 'always) ;; Allow shift-selection in org-mode

;;
;; Highlight current line number
;;
(use-package hlinum)
(hlinum-activate)
(setq linum-highlight-in-all-buffersp t)

;;
;; For moving buffers around
;;
(use-package buffer-move)

(global-set-key (kbd "<f9>") 'revert-buffer)

;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
                (lambda () (interactive)
                  (condition-case nil (scroll-down)
                    (beginning-of-buffer (goto-char (point-min))))))

;;
;;  Mode-line configuration
;;
(defun add-current-dictionary ()
  "Show the current dictionary in the mode-line."
  (add-to-list 'mode-line-buffer-identification 
               '(:eval (concat ispell-current-dictionary " "))))

(add-hook 'text-mode-hook 'add-current-dictionary)

;; Setup smart-mode-line
(use-package smart-mode-line)
(sml/setup)

;;
;; Magit
;;
(use-package magit)
(setq magit-repository-directories '("~/git-repos/" "~/git-repos/ovt/externals/" "~/.emacs.d/"))
(global-set-key (kbd "C-x g") 'magit-status)

;;
;; Validate commit messages
;;
(use-package git-commit)

;;
;; Highlight changes in the fringe
;;
(use-package diff-hl
  :config
  (global-diff-hl-mode))

;;
;; Navigation
;;
(require 'navigation-config-pvj)
(global-set-key (kbd "<f1>")   'pvj/copy-file-path)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "<home>") 'pvj/smart-move-to-line-beginning)
(global-set-key (kbd "C-a")    'pvj/smart-move-to-line-beginning)
(global-set-key (kbd "C-z")    'delete-other-windows)
(global-set-key (kbd "C-x o") 'ace-window)

;;
;; Support for Python
;;
(use-package elpy)
(elpy-enable)

;;
;; Diminish
;;
(require 'diminish-config-pvj)

;;
;; Utility functions
;;
(require 'util-pvj)
(global-set-key (kbd "<f5>") 'pvj/toggle-window-split)
