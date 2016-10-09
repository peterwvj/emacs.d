;;
;; eshell configuration
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
