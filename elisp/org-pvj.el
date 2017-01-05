;;
;; org-mode configuration
;;

(use-package org
  :config
  ;; Allow shift-selection in org-mode
  (setq org-support-shift-select 'always) 

  (setq org-agenda-files '("~/agenda.org")) 


  (setq org-src-tab-acts-natively t)

  (setq org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                               (vm-imap . vm-visit-imap-folder-other-frame)
                               (gnus . org-gnus-no-new-news)
                               ;; Open file in current window
                               (file . find-file)
                               (wl . wl-other-frame)))

  ;; Combine output from Org with the diary.
  (setq org-agenda-include-diary t)

  ;;
  ;; Show org-mode bullets as UTF-8 characters
  ;;
  (use-package org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(provide 'org-pvj)
