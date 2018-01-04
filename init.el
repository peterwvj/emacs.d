
;; Increase the buffer size of *Messages*
(setq message-log-max 20000)

;; Increase the garbage collection threshold during startup
(defconst pvj/default-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 256 1024 1024))
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold pvj/default-gc-cons-threshold)))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Use 'use-package' to manage packages
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :config
  (setq use-package-always-ensure t)
  (setq use-package-always-demand t)
  (setq use-package-always-pin "melpa"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Convenient way to tell emacs 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'startup-messages-config-pvj)
(require 'file-config-pvj)
(require 'locale-config-pvj)
(require 'font-config-pvj)
(require 'scratch-config-pvj)
(require 'version-control-config-pvj)
(require 'file-explorer-config-pvj)
(require 'grep-config-pvj)
(require 'helm-config-pvj)
(require 'pdf-tools-config-pvj)
(require 'latex-config-pvj)
(require 'text-config-pvj)
(require 'org-config-pvj)
(require 'buffers-config-pvj)
(require 'eshell-config-pvj)
(require 'mu4e-config-pvj)
(require 'window-config-pvj)
(require 'company-config-pvj)
(require 'mode-line-config-pvj)
(require 'navigation-config-pvj)
(require 'diminish-config-pvj)
(require 'news-config-pvj)
(require 'browser-config-pvj)
(require 'programming-config-pvj)
(require 'flycheck-config-pvj)
(require 'visual-config-pvj)
(require 'hydra-config-pvj)
(require 'restart-config-pvj)
(require 'util-pvj)

(provide 'init)
