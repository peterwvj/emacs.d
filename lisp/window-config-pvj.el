;;
;; Windows configuration
;;

;; Manual control using the golden-ratio command
(use-package golden-ratio)

;; Also applies to new clients that connect to the Emacs server
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Manage multiple window configurations.
(use-package elscreen
  :config
  (progn
    (set-face-attribute 'elscreen-tab-background-face nil :inherit 'default :background nil)
    (setq-default elscreen-tab-display-control nil)
    (setq-default elscreen-tab-display-kill-screen nil)
    (setq-default elscreen-display-screen-number nil)
    (setq-default elscreen-display-tab nil)

    (use-package helm-elscreen
      :bind
      (:map elscreen-map
            ("h" . helm-elscreen)))
    
    (elscreen-set-prefix-key "\C-cs")

    (global-set-key [C-prior] 'elscreen-previous)
    (global-set-key [C-next] 'elscreen-next)
    
    (elscreen-start)))

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

(provide 'window-config-pvj)
