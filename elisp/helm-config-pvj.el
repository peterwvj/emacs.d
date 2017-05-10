;;
;; helm configuration
;;

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ;; Mark buffers with C-SPACE and kill them with M-D
   ("C-x C-b" . helm-buffers-list))
  :config
  (helm-mode 1)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  (setq helm-ff-newfile-prompt-p nil)
  ;; I used to have a problem where C-x C-f, i.e. helm-find-files would make emacs hang.
  ;; Workaround: by setting this flag I can prevent Emacs from hanging...
  (setq helm-split-window-in-side-p t))

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;
;; Helm interface for Emacs 'describe-bindings
;;
(use-package helm-descbinds
  :config
  (helm-descbinds-mode))

;;
;; Helm interface for flyspell
;;
(use-package helm-flyspell
  :bind
  (:map flyspell-mode-map
        ("C-;" . helm-flyspell-correct)))

;; helm interface for projectile
(use-package helm-projectile
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  ;; Show project status
  (setq projectile-switch-project-action 'projectile-vc)
  (helm-projectile-on))

;; helm swoop
(use-package helm-swoop
  :bind
  (("M-i"     . helm-swoop)
   ("M-I"     . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all))
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map)
  ;; From helm-swoop to helm-multi-swoop-all
  (bind-key "M-i" #'helm-multi-swoop-all-from-helm-swoop
            helm-swoop-map)
  ;; Move up and down like isearch
  (bind-keys :map helm-swoop-map
             ("C-r" . helm-previous-line)
             ("C-s" . helm-next-line))

  (bind-keys :map helm-multi-swoop-map
             ("C-r" . helm-previous-line)
             ("C-s" . helm-next-line))
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
  (setq helm-swoop-use-fuzzy-match t))

;; Use 'helm-make' to list targets
(use-package helm-make)

(provide 'helm-config-pvj)
