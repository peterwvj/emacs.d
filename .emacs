
(setq package-archives '(
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(add-to-list 'load-path "~/git-repos/emacs-config-pvj/elisp/")

;; Get rid of the splash screen and echo area message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;;
;; Use persistent scratch buffer
;;
(setq initial-scratch-message "")
(persistent-scratch-setup-default)

;;
;; Remember recently opened files
;;
(require 'recentf)
(setq recentf-exclude '("/Maildir/" "/.emacs.d/" ".aspell."))
(setq recentf-max-saved-items 50)

;; C-x R to view recently opened files
(require 'dired+)
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)
(setq dired-listing-switches "-alh")
(define-key dired-mode-map (kbd "M-s") 'diredp-find-a-file)

;;
;; Configure helm mode
;;
(require 'helm-config)
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
(require 'helm-descbinds)
(helm-descbinds-mode)

;;
;; Enable winner-mode
;;
(winner-mode 1)

;;
;; Enable undo-tree
;;
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
(require 'pdf-tools)
(pdf-tools-install)

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
(global-set-key (kbd "<f8>")   'pvj/switch-dictionary)
(global-set-key (kbd "C-q") 'pvj/duplicate-current-line-or-region)
(global-set-key (kbd "C-d") 'kill-whole-line)
(delete-selection-mode 1)
(global-set-key (kbd "C-w") 'pvj/kill-word-or-region)
(global-set-key (kbd "<f6>") 'browse-kill-ring)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)


;;
;; For language analysis
;;
(require 'langtool)
(setq langtool-language-tool-jar "/home/peter/tools/LanguageTool-3.4/languagetool-commandline.jar")

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


;;
;; Highlights delimiters such as parentheses, brackets or braces according to their depth
;;
(require 'rainbow-delimiters)
;; To start the mode automatically in most programming modes (Emacs 24 and above):
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;
;; No blinking cursor
;;
(blink-cursor-mode 0)

;; Use the naquadah theme. In addition, the cursor color has been
;; changed to green, and the text-selection color (the region face)
;; has been changed to blue
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
(require 'move-text)
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
(scroll-bar-mode -1)

;;
;; Alarm bell config
;;
(setq ring-bell-function 'ignore) ;; Completely turn off the alarm bell


;; go-up for eshell
(require 'eshell-up)

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
(require 'color)

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
(require 'mu4e-config-pvj)
(global-set-key (kbd "<f2>") 'mu4e)

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
(require 'smtpmail)

;; Use same authinfo file for work and private emails
(setq message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
      smtpmail-debug-info t)

;;
;; Scrolling
;;
;; scroll-conservatively setting: Only scroll one line when moving the cursor past the top or bottom of the window
;; Other settings: help avoid the "text jumps"
;; See - https://www.emacswiki.org/emacs/SmoothScrollign
(setq scroll-margin 1
scroll-conservatively 10000 
scroll-up-aggressively 0.01
scroll-down-aggressively 0.01)

;; Also applies to new clients that connect to the Emacs server
(add-to-list 'default-frame-alist '(fullscreen . maximized)) 

;;
;; Auto-completion configuration
;;
(require 'auto-complete-config-pvj)

;;
;; org-mode
;;
(setq org-support-shift-select 'always) ;; Allow shift-selection in org-mode

;;
;; Highlight current line number
;;
(require 'hlinum)
(hlinum-activate)
(setq linum-highlight-in-all-buffersp t)

;;
;; For moving buffers around
;;
(require 'buffer-move)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfw:face-header ((t (:foreground "green" :weight bold))))
 '(cfw:face-holiday ((t (:background "grey10" :foreground "deep sky blue" :weight bold))))
 '(cfw:face-select ((t (:background "dim gray"))))
 '(cfw:face-title ((t (:inherit variable-pitch :foreground "white" :weight bold :height 2.0))))
 '(cfw:face-toolbar ((t nil)))
 '(cursor ((t (:background "lime green"))))
 '(eshell-prompt ((t (:foreground "green yellow" :weight bold))))
 '(mu4e-replied-face ((t (:inherit font-lock-builtin-face :foreground "yellow" :weight normal))))
 '(region ((t (:background "DeepSkyBlue4"))))
 '(term-color-blue ((t (:background "deep sky blue" :foreground "deep sky blue")))))

;;
;; Calendar configuration
;;
(require 'calfw-config-pvj)
(global-set-key (kbd "<f10>")   'show-calendar)

;;
;;  Mode-line configuration
;;
(defun add-current-dictionary ()
  "Show the current dictionary in the mode-line."
  (add-to-list 'mode-line-buffer-identification 
               '(:eval (concat ispell-current-dictionary " "))))

(add-hook 'text-mode-hook 'add-current-dictionary)

;; Setup smart-mode-line
(sml/setup)

;;
;; Magit
;;
(setq magit-repository-directories '("~/git-repos/" "~/git-repos/ovt/externals/"))
(global-set-key (kbd "C-x g") 'magit-status)

;;
;; Navigation
;;
(require 'navigation-config-pvj)
(global-set-key (kbd "<f1>")   'pvj/copy-file-path)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "<home>") 'pvj/smart-move-to-line-beginning)
(global-set-key (kbd "C-a")    'pvj/smart-move-to-line-beginning)
(global-set-key (kbd "C-z")    'delete-other-windows)

;;
;; Support for Python
;;
(elpy-enable)

;;
;; Utility functions
;;
(require 'util-pvj)
(global-set-key (kbd "<f5>") 'pvj/toggle-window-split)
