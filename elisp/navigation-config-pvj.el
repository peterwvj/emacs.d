
;; Copy file path
(defun pvj/copy-file-path ()
  (interactive)
  (let ((kill (if (null buffer-file-name)
                  (kill-new default-directory)
                (kill-new buffer-file-name) )))
    (message (concat "Latest kill: " kill))))

(add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode-20140616.115/ace-jump-mode.el/in")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(require 'ace-window)
(global-set-key (kbd "M-p") 'ace-window)

;; Inspired by http://stackoverflow.com/questions/145291/smart-home-in-emacs/
(defun pvj/smart-move-to-line-beginning ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^") ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(provide 'navigation-config-pvj)
