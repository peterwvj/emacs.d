;;
;; Font configuration
;;

(defun pvj/setup-font ()
  (let ((fonts (font-family-list)))
    (cond ((member "Iosevka" fonts)
           (set-frame-font "Iosevka-14"))
          ((member "Source Code Pro" fonts)
           (set-frame-font "Source Code Pro-12"))
          ((member "DejaVu Sans Mono" fonts)
           (set-frame-font "DejaVu Sans Mono-12")))))

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
