;;
;; Configuration related to buffers
;;

;; Kill buffers that have a live process attached - without asking!
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;;
;; Better handling of temporary buffers
;;
(use-package popwin
  :config
  (popwin-mode 1))


(provide 'buffers-config-pvj)
