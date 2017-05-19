
;;
;; Locale related configuration
;;

;; Prefer UTF-8 when no encoding is specified
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))
(prefer-coding-system 'utf-8)

(provide 'locale-config-pvj)
