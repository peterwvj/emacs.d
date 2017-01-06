
(use-package ace-window
  :config
  (setq aw-scope 'frame))

(use-package ace-jump-mode)

;; Copy file path
(defun pvj/copy-file-path ()
  (interactive)
  (let ((kill (if (null buffer-file-name)
                  (kill-new default-directory)
                (kill-new buffer-file-name) )))
    (message (concat "Latest kill: " kill))))

;; Inspired by http://stackoverflow.com/questions/145291/smart-home-in-emacs/
(defun pvj/smart-move-to-line-beginning ()
  "Move point to first non-whitespace character or
beginning-of-line. Note that if the string begins with ^ and
shift-select-mode' is non-nil, Emacs first calls the function
`handle-shift-select'"
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(provide 'navigation-config-pvj)
