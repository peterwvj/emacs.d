;;
;; Auto-completion configuration
;;
(require 'auto-complete)
(require 'auto-complete-config)

;; Additional proposals can be added to ~/.dict. Each entry must be
;; listed on a separate line.

;; Basic auto-completion config
(ac-config-default)

(define-key ac-complete-mode-map [tab] 'ac-expand)
(ac-set-trigger-key "<tab>")

;; Use UP and DOWN keys to select proposals
(define-key ac-complete-mode-map [down] 'ac-next)
(define-key ac-complete-mode-map [up] 'ac-previous)

(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'text-mode)

(require 'ac-math) 
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

;; Fix for issue with line numbers and auto-completion. See
;; http://stackoverflow.com/questions/11484225/fix-an-auto-complete-mode-and-linum-mode-annoyance
(ac-linum-workaround)

(provide 'auto-complete-config-pvj)
