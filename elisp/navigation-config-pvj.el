
;;
;; Navigation related configuration
;;


;;
;; Enable winner-mode
;;
(winner-mode 1)

;;
;; Convenient way to move between windows
;;
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;;
;; Scrolling
;;
;; This seems to be the best way to achieve "smooth scrolling"
;; See - https://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-conservatively 10000)

;;
;; For moving buffers around
;;
(use-package buffer-move)

;; Page down/up move the point, not the screen.
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
                (lambda () (interactive)
                  (condition-case nil (scroll-down)
                    (beginning-of-buffer (goto-char (point-min))))))

(use-package ace-window
  :config
  (setq aw-scope 'frame))


(use-package ace-jump-mode)

;; Display key bindings
(use-package which-key
  :config
  (which-key-mode))

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

(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "<home>") 'pvj/smart-move-to-line-beginning)
(global-set-key (kbd "C-a")    'pvj/smart-move-to-line-beginning)
(global-set-key (kbd "C-z")    'delete-other-windows)
(global-set-key (kbd "C-x o") 'ace-window)

(provide 'navigation-config-pvj)
