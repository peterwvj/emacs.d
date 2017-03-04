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

(provide 'util-pvj)
