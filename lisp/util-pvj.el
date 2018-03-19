;;
;; Utility functions
;;
(defun pvj/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun pvj/unfill-paragraph (&optional region)
  "Change multi-line paragraph to a single line of text.
Argument REGION the paragraph region."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;; Inspired by https://gist.github.com/hyOzd/23b87e96d43bca0f0b52
(defun pvj/delete-file-and-buffer ()
  "Deletes current file and kill associated buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file!"))))

(defun pvj/create-buffer-copy ()
  (interactive)
  (let ((buf (current-buffer))
        (win (get-buffer-window))
        (name (generate-new-buffer-name "BUFFERCOPY")))
    (generate-new-buffer name)
    ;; (get-buffer-create name)
    (copy-to-buffer name (point-min) (point-max))
    (split-window-horizontally)
    (other-window 1)
    (switch-to-buffer name)
    (other-window -1)))

(defun pvj/open-with (arg)
  "Open visited file in default external program.

With a prefix ARG always prompt for command to use."
  (interactive "P")
  (when buffer-file-name
    (shell-command (concat
                    (cond
                     ((and (not arg) (eq system-type 'darwin)) "open")
                     ((and (not arg) (member system-type '(gnu gnu/linux gnu/kfreebsd))) "xdg-open")
                     (t (read-shell-command "Open current file with: ")))
                    " "
                    (shell-quote-argument buffer-file-name)))))

(defun pvj/open-gnome-terminal ()
  (interactive)
  (shell-command "gnome-terminal"))

(provide 'util-pvj)
