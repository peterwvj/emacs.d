
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
;; File configuration
;;
(require 'file-config-pvj)

;;
;; Locale
;;
(require 'locale-config-pvj)

;; Set the custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;
;; Convenient way to tell emacs 'yes' or 'no'
;;
(defalias 'yes-or-no-p 'y-or-n-p)

;; Get rid of the splash screen and echo area message
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;;
;; Scratch buffer configuration
;;
(require 'scratch-config-pvj)

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
;; Buffer related configuration
;;
(require 'buffers-config-pvj)

;;
;; To avoid typing ESC-ESC-ESC to escape or quit
;;
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;
;; eshell configuration
;;
(require 'eshell-config-pvj)

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
;; News feeds
;;
(require 'news-config-pvj)

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

;;
;; Syntax checking
;;
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(require 'programming-config-pvj)

(require 'visual-config-pvj)
