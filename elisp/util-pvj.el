;;
;; Utility functions
;;

(defun pvj/get-file-contents (filePath)
  "Return the contents of a file as a string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun pvj/copy-file-contents (filePath)
  "Copy the contents of a file to the kill ring."
  (let ((contents (pvj/get-file-contents filePath)))
    (progn
      (kill-new contents)
      filePath)))

(defun pvj/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun pvj/show-agenda ()
  (interactive)
  (delete-other-windows)
  (find-file "~/agenda.org")
  (org-agenda-list)
  (other-window -1))

(defun pvj/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(provide 'util-pvj)
