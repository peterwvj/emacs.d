;;
;; Calendar configuration
;;
(use-package calfw
  :config 
  (use-package calfw-ical
    :ensure f))

(defun pvj/text-from-file (file)
  "Return a file contant as a string"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; The calendars are stored in .ICS format
(defun show-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    ;; Private calendar
    (cfw:ical-create-source "Private (Gmail)" (pvj/text-from-file "~/.private-calendar") "DarkOrange")
    ;; Work calendar
    (cfw:ical-create-source "Work" (pvj/text-from-file "~/.work-calendar") "White")
    )))

(provide 'calfw-config-pvj)
