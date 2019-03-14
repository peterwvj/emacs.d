
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
  (setq magit-repository-directories `("~/git-repos/" "~/git-repos/ovt/externals/" ,user-emacs-directory))
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

;; Show TODOs and FIXMEs in the Magit status buffer
(use-package magit-todos
  :config
  (add-hook 'magit-status-mode-hook #'magit-todos-mode))

;;
;; Validate commit messages
;;
(use-package git-commit)


;;
;; Walk through revisions of a file
;;
(use-package git-timemachine)

;;
;; For editing .gitignore files
;;
(use-package gitignore-mode)

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
