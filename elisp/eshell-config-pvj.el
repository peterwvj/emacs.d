;;
;; eshell configuration
;;

;;
;; eshell config inspired by https://github.com/porterjamesj/.emacs.d/blob/master/user-lisp/setup-eshell.el
;;

(require 'eshell)
(require 'xterm-color)

(setq eshell-banner-message "")

(setq eshell-aliases-file "~/.eshell.aliases")

(defun eshell-clear-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; Coloring output
(add-hook 'eshell-mode-hook
          (lambda () (progn
            (setq xterm-color-preserve-properties t)
            (setenv "TERM" "xterm-256color")
            ;; Clearing the buffer ensures that the prompt is rendered
            ;; using the desired faces
            (eshell-clear-buffer)
            )))

(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)

(setq eshell-output-filter-functions
    (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

(add-hook 'eshell-mode-hook
          '(lambda()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)
             (local-set-key (kbd "C-u") (lambda ()
                                          (interactive)
                                          (progn 
                                            (end-of-buffer)
                                            (eshell-bol)
                                            (kill-visual-line))))))

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

;;
;; Git Completion for eshell - inspired by https://tsdh.wordpress.com/2013/05/31/eshell-completion-for-git-bzr-and-hg/
;;
(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "available git commands in")
    (let (commands)
      (while (re-search-forward
	      "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
	      nil t)
	(push (match-string 1) commands)
	(when (match-string 2)
	  (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
	(push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
			     (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

(setq eshell-scroll-to-bottom-on-input t)

;; Jump to recently visited directories
(require 'eshell-z)

(provide 'eshell-config-pvj)
