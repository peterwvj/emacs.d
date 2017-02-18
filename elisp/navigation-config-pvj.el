
(use-package ace-window
  :config
  (setq aw-scope 'frame))

(use-package ace-jump-mode)

(defun pvj/copy-file-path ()
  "Copy the absolute path of the current file."
  (interactive)
  (let ((kill (if (null buffer-file-name)
                  (kill-new default-directory)
                (kill-new buffer-file-name) )))
    (message (concat "Latest kill: " kill))))

;; Inspired by http://stackoverflow.com/questions/145291/smart-home-in-emacs/
(defun pvj/smart-move-to-line-beginning ()
  "Move point to first non-white-space character or 'beginning-of-line'."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(provide 'navigation-config-pvj)
