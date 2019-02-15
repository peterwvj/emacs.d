;;
;; company-mode configuration
;;

(use-package company
  :config

  (setq company-idle-delay 0.0)
  (setq company-minimum-prefix-length 1)

  ;; Tell dabbrev not to downcase candidates. See
  ;; https://emacs.stackexchange.com/questions/10837/how-to-make-company-mode-be-case-sensitive-on-plain-text
  (setq company-dabbrev-downcase nil)
  
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-C-/") 'company-complete)

  (setq company-show-numbers t)

  ;; Remove irrelevant suggestions from generated LaTeX related
  ;; files. For details, see issue
  ;; https://github.com/syl20bnr/spacemacs/issues/9706
  (require 'company-files)
  (dolist (extension '(".fbd_latexmk"
                       ".aux"
                       ".log"
                       ".pdf"
                       ".bbl"
                       ".bcf"
                       ".gz"
                       ".blg"
                       ".fls"))
    (push extension company-files-exclusions)))

;;
;; Rank candidates based on statistics
;;
(use-package company-statistics
  :config
  (company-statistics-mode))

;; (use-package company-flx
;;   :config
;;   (with-eval-after-load 'company
;;     (company-flx-mode +1)))

;;
;; Show documentation popups when idling on a completion candidate
;;
(use-package company-quickhelp
  :config
  (setq company-quickhelp-delay 0.1)
  (company-quickhelp-mode 1))

(provide 'company-config-pvj)
