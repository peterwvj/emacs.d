;;
;; eshell configuration
;;

;;
;; eshell config inspired by https://github.com/porterjamesj/.emacs.d/blob/master/user-lisp/setup-eshell.el
;;

(use-package xterm-color)

(use-package eshell
  :init
  (defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))
  :config
  (setq eshell-banner-message "")

  (setq eshell-aliases-file (expand-file-name ".eshell.aliases" user-emacs-directory))

  (defun pvj/eshell-clear-buffer ()
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  ;; Coloring output
  (add-hook 'eshell-mode-hook
            (lambda () (progn
                    (beacon-mode -1)
                    (setq xterm-color-preserve-properties t)
                    (setenv "TERM" "xterm-256color")
                    ;; Clearing the buffer ensures that the prompt is rendered
                    ;; using the desired faces
                    (pvj/eshell-clear-buffer))))

  (require 'esh-mode)
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)

  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

  (add-hook 'eshell-mode-hook
            '(lambda()
               (local-set-key (kbd "C-l") 'pvj/eshell-clear-buffer)
               (local-set-key (kbd "<home>") 'eshell-bol)
               (local-set-key (kbd "C-u") (lambda ()
                                            (interactive)
                                            (progn
                                              (end-of-buffer)
                                              (eshell-bol)
                                              (kill-visual-line))))))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (face-remap-add-relative 'default '(:foreground "green yellow"))))

  (defun pvj/curr-dir-git-branch (pwd)
    "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
    (interactive)
    (if (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
        (let ((git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD"))))
          (propertize
           (concat "[±:" (substring git-output 0 -1) "]")
           'face `(:foreground "#D1D62D")))
      (propertize
       "[±]" 'face `(:foreground "#555555"))))

  (defun pvj/eshell-prompt ()
    (let ((header-bg "#fff"))
      (concat
       (with-face user-login-name '(:foreground "orange" :weight bold))
       "@"
       (with-face (concat (system-name) ":") '(:foreground "yellow" :weight bold))
       (with-face (abbreviate-file-name (eshell/pwd)) '(:foreground "LightSkyBlue" :weight bold))
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

  (add-hook 'eshell-mode-hook (lambda ()
                                (setq eshell-pwd-convert-function (lambda (f)
                                                                    (if (file-equal-p (file-truename f) "/")
                                                                        "/" f)))
                                (setq eshell-cmpl-ignore-case t
                                      eshell-hist-ignoredups t)
                                ;; Use helm completion in eshell
                                ;; (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                                (setq pcomplete-cycle-completions nil)
                                ;; Use helm to browse history
                                (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)))

  (setq eshell-highlight-prompt t)
  (setq eshell-prompt-regexp "\λ ")

  (defun pvj/eshell-here ()
    "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-width) 2))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-horizontally (- height))
      ;;(other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))

      (insert (concat "ls"))
      (eshell-send-input)))

  (setq eshell-scroll-to-bottom-on-input t)

  ;; Quickly go to a specific parent directory
  (use-package eshell-up)
  
  ;; Jump to recently visited directories
  (use-package eshell-z)

  (global-set-key (kbd "<f3>") 'eshell)
  (global-set-key (kbd "<f4>") 'pvj/eshell-here))

(provide 'eshell-config-pvj)
