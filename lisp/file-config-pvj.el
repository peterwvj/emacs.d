;;
;; File access configuration
;;

(setq load-prefer-newer t)

;;
;; Remember recently opened files
;;
(setq recentf-exclude '("/Maildir/" ".aspell."))
(setq recentf-max-saved-items 50)

;;
;; Put backup files in same directory (to avoid having emacs creating files everywhere)
;;
(setq backup-directory-alist `(("." . ,(expand-file-name ".emacs-backup" user-emacs-directory))))

;; If a file or buffer does not exist when using C-x C-f or C-x b then
;; skip the confirmation
(setq confirm-nonexistent-file-or-buffer nil)

;; Always follow symlinks
(setq vc-follow-symlinks t)

(global-set-key (kbd "C-c v") 'view-file)

(provide 'file-config-pvj)
