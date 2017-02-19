;;
;; Utility functions
;;
(defun pvj/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

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

(global-set-key (kbd "<f5>") 'pvj/toggle-window-split)

(provide 'util-pvj)
