;;
;; Grep configuration
;;

(defun rgrep-fullscreen () 
  "Run `rgrep' in full screen."
  (interactive)
  (call-interactively 'rgrep)
  (select-window (get-buffer-window "*grep*"))
  (delete-other-windows))


;; Writable grep buffer
(use-package wgrep)

(provide 'grep-config-pvj)
