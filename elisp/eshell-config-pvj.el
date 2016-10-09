;;
;; eshell configuration
;;

;;
;; eshell config inspired by https://github.com/porterjamesj/.emacs.d/blob/master/user-lisp/setup-eshell.el
;;

(setq eshell-banner-message "")

(setq eshell-aliases-file "~/.eshell.aliases")

(defun eshell-clear-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          '(lambda()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (linum-mode -1)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local global-hl-line-mode nil)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (face-remap-add-relative 'default '(:foreground "green yellow"))))

(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun pvj/curr-dir-git-branch (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (if (and (eshell-search-path "git")
           (locate-dominating-file pwd ".git"))
      (let ((git-output (shell-command-to-string (concat "git branch | grep '\\*' | sed -e 's/^\\* //'"))))
        (propertize
         (concat "[±:"
                 (if (> (length git-output) 0)
                     (substring git-output 0 -1)
                   "no branch")
                 "]")
         'face `(:foreground "#D1D62D")))
    (propertize
     "[±]" 'face `(:foreground "#555555"))))

(defun pvj/eshell-prompt ()
  (let ((header-bg "#fff"))
    (concat
     (with-face
      (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
      :background header-bg)
     (with-face user-login-name '(:foreground "orange" :weight bold))
     "@"
     (with-face (concat (system-name) ":") '(:foreground "yellow" :weight bold))
     (with-face (eshell/pwd) '(:foreground "LightSkyBlue" :weight bold))
     (with-face (pvj/curr-dir-git-branch (eshell/pwd)) '(:foreground "white" :weight bold))
     (if (= (user-uid) 0)
       (with-face " #" :foreground "red")
       "\nλ "))))

(setq eshell-prompt-function 'pvj/eshell-prompt)

(add-hook 'eshell-mode-hook
(lambda ()
  (defun eshell-emit-prompt ()
    "Emit a prompt if eshell is being used interactively. I
am redefining it here so that it doesn't screw up my colors"
    (run-hooks 'eshell-before-prompt-hook)
    (if (not eshell-prompt-function)
        (set-marker eshell-last-output-end (point))
      (let ((prompt (funcall eshell-prompt-function)))
        (and eshell-highlight-prompt
             (add-text-properties 0 (length prompt)
                                  '(read-only t
                                              ;; face eshell-prompt
                                              rear-nonsticky (face read-only))
                                  prompt))
        (eshell-interactive-print prompt)))
    (run-hooks 'eshell-after-prompt-hook))))

(setq eshell-highlight-prompt t)
(setq eshell-prompt-regexp "\λ ")

(provide 'eshell-config-pvj)
