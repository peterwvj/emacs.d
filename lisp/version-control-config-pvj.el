
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
  (setq magit-repository-directories `("~/git-repos/" "~/git-repos/ovt/externals/" ,user-emacs-directory))
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-define-popup-switch 'magit-push-popup ?u "Set upstream" "--set-upstream"))

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
(use-package gitignore-mode
  :config
  (add-hook 'gitignore-mode-hook
            (lambda ()
              (electric-indent-mode -1))))

;;
;; Highlight changes in the fringe
;;
(use-package diff-hl
  :config
  (global-diff-hl-mode))

;;
;; Subverion
;;
(use-package dsvn)

;;
;; ediff configuration
;;
;; To make ‘ediff’ operate on selected-frame use the following:
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; To make ediff to be horizontally split use:
(setq ediff-split-window-function 'split-window-horizontally)

(provide 'version-control-config-pvj)
