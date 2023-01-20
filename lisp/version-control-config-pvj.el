
;;
;; Version control related configuration
;;

;;
;; Git Porcelain inside Emacs
;;
(use-package magit
  :bind
  (("C-x g" . magit-status))
  :config
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  ;; Show recent unpushed and unpulled commits
  (setq magit-section-initial-visibility-alist '((unpushed . show)
                                                 (unpulled . show)))
  (setq magit-repository-directories `("~/git/" ,user-emacs-directory))
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; Show TODOs and FIXMEs in the Magit status buffer

(when (version<= "25.2" emacs-version)
  (use-package magit-todos
    :config (magit-todos-mode)))
;;
;; Validate commit messages
;;
(use-package git-commit)


;;
;; Walk through revisions of a file
;;
(use-package git-timemachine)

;;
;; Highlight changes in the fringe
;;
(use-package diff-hl
  :config
  (global-diff-hl-mode))

;;
;; ediff configuration
;;
;; To make ‘ediff’ operate on selected-frame use the following:
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; To make ediff to be horizontally split use:
(setq ediff-split-window-function 'split-window-horizontally)

(provide 'version-control-config-pvj)
