;;
;; Windows configuration
;;

;; Also applies to new clients that connect to the Emacs server
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Manage multiple window configurations.
(use-package eyebrowse
  :config
  (setq eyebrowse-wrap-around t
        eyebrowse-new-workspace t)
  (eyebrowse-mode t))

(defun pvj/toggle-window-split ()
  "Toggle between vertical and horizontal splitting of two windows."
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

(global-set-key (kbd "<f5>") 'pvj/toggle-window-split)

(defun pvj/split-switch-window-vertically ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil))

(defun pvj/split-switch-window-horizontally ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil))
 
(global-set-key (kbd "C-x 2") 'pvj/split-switch-window-vertically)
(global-set-key (kbd "C-x 3") 'pvj/split-switch-window-horizontally)

(provide 'window-config-pvj)
