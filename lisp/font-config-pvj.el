;;
;; Font configuration
;;

(defun pvj/setup-font ()
  (if (member "DejaVu Sans Mono" (font-family-list))
      (set-frame-font "DejaVu Sans Mono-14")))

(defun pvj/configure-frame (frame)
  "Configure faces on frame creation"
  (select-frame frame)
  (if (display-graphic-p)
      (pvj/setup-font)
    (progn
      ;; set up different stuff for text console
      )))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'pvj/configure-frame)
  (pvj/setup-font))

(provide 'font-config-pvj)
