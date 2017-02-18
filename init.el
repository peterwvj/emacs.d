
;; Increase the buffer size of *Messages*
(setq message-log-max 20000)

;; Increase the garbage collection threshold during startup
(defconst pvj/default-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 256 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold pvj/default-gc-cons-threshold)))

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

(use-package use-package
  :config
  (setq use-package-always-ensure t)
  (setq use-package-always-demand t)
  (setq use-package-always-pin "melpa"))

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;;
;; Compile Elisp sources automatically
;;
(setq load-prefer-newer t)
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;
;; Locale
;;
(require 'locale-config-pvj)

;; Set the custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Get rid of the splash screen and echo area message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;;
;; Scratch buffer configuration
;;
(require 'scratch-config-pvj)

;;
;; Remember recently opened files
;;
(setq recentf-exclude '("/Maildir/" ".aspell."))
(setq recentf-max-saved-items 50)

;;
;; Version control
;;
(require 'version-control-config-pvj)

;;
;; File explorer configuration
;;
(require 'file-explorer-config-pvj)

;;
;; Helm configuration
;;
(require 'helm-config-pvj)

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
    (nlinum-mode -1)))

;;
;; Functionality that supports text editing
;;
(require 'text-config-pvj)

;;
;; org-mode configuration
;;
(require 'org-config-pvj)

;;
;; Put backup files in same directory (to avoid having emacs creating files everywhere)
;;
(setq backup-directory-alist `(("." . ,(expand-file-name ".emacs-backup" user-emacs-directory))))

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

;;
;; To avoid typing ESC-ESC-ESC to escape or quit
;;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Always follow symlinks
(setq vc-follow-symlinks t)

;;
;; eshell configuration
;;
(require 'eshell-config-pvj)

;;
;; ansi-term config
;;

;; Start ansi-term in /bin/bash/
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;;
;; External browser configuration (use Google Chrome)
;;
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;;
;; mu4e configuration
;;
(use-package mu4e-config-pvj
  :if (and (string-equal system-type "gnu/linux") (null noninteractive))
  :ensure f
  :bind (("<f2>" . mu4e))
  :config
  (progn
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

;; Also applies to new clients that connect to the Emacs server
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'company-config-pvj)

;; Setup smart-mode-line
(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

;;
;; Navigation
;;
(require 'navigation-config-pvj)

;;
;; Diminish
;;
(require 'diminish-config-pvj)

;;
;; Web feed reader
;;
(use-package elfeed
  :bind
  (:map elfeed-search-mode-map
        ("U" . elfeed-search-fetch))
  :config
  (setq elfeed-feeds
        '(("https://newz.dk/rss" it)
          ("http://planet.emacsen.org/atom.xml" emacs)
          ("http://feeds.tv2.dk/nyheder/rss" tv2))))

;;
;; Launch google searches from within Emacs
;;
(use-package google-this
  :config
  (google-this-mode 1))

;;
;; Utility functions
;;
(require 'util-pvj)
(global-set-key (kbd "<f5>") 'pvj/toggle-window-split)

(use-package google-translate
  :config
  (setq google-translate-default-source-language "da")
  (setq google-translate-default-target-language "en"))

;;
;; Better handling of temporary buffers
;;
(use-package popwin
  :config
  (popwin-mode 1))

;;
;; Syntax checking
;;
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

;; Display key bindings
(use-package which-key
  :config
  (which-key-mode))

(require 'programming-config-pvj)

(require 'visual-config-pvj)
