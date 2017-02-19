
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

;; Set the custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;
;; Convenient way to tell emacs 'yes' or 'no'
;;
(defalias 'yes-or-no-p 'y-or-n-p)


(require 'startup-messages-config-pvj)

;;
;; File configuration
;;
(require 'file-config-pvj)

;;
;; Locale
;;
(require 'locale-config-pvj)

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

(require 'pdf-tools-config-pvj)

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
;; mu4e configuration
;;
(require 'mu4e-config-pvj)

(require 'window-config-pvj)

(require 'company-config-pvj)

;;
;; Mode-line configuration
;;
(require 'mode-line-config-pvj)

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

(require 'browser-config-pvj)

(require 'programming-config-pvj)

(require 'flycheck-config-pvj)

(require 'visual-config-pvj)

;;
;; Utility functions
;;
(require 'util-pvj)

