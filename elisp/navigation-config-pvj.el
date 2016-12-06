
(use-package ace-window)
(setq aw-scope 'frame)

;; Copy file path
(defun pvj/copy-file-path ()
  (interactive)
  (let ((kill (if (null buffer-file-name)
                  (kill-new default-directory)
                (kill-new buffer-file-name) )))
    (message (concat "Latest kill: " kill))))

(use-package ace-jump-mode)

;; Inspired by http://stackoverflow.com/questions/145291/smart-home-in-emacs/
(defun pvj/smart-move-to-line-beginning ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^") ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(provide 'navigation-config-pvj)
