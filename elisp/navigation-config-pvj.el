
;; Copy file path
(defun pvj/copy-file-path ()
  (interactive)
  (let ((kill (if (null buffer-file-name)
                  (kill-new default-directory)
                (kill-new buffer-file-name) )))
    (message (concat "Latest kill: " kill))))

(defun set-default-directory ()
  (interactive)
  (let ((r (completing-read  "Set default directory: " '(("~/svn-repos/joint/phds/" "phds") ("~/svn-repos/joint/phds/PeterJorgensen/" "pvj")) nil t nil)))
    (setq default-directory r)))

(add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode-20140616.115/ace-jump-mode.el/in")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(provide 'navigation-config-pvj)
