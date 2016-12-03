
(use-package tex
  :ensure auctex)

;;
;; Set AuxTex to PDF mode
;;
(setq TeX-PDF-mode t)

;; Generate sync file and sync with C-v
(eval-after-load
    "tex" '(add-to-list 'TeX-command-list
                        '("latexmk" "latexmk -pdf %t --synctex=1" TeX-run-TeX)))

(setq latex-run-command "pdflatex")
(setq LaTeX-command "latex --synctex=1")

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t
      TeX-source-correlate-method 'synctex)
;;; AUCTeX config
(setq TeX-auto-save t
      TeX-parse-self t)

;; Needed to sync TeX and PDF
(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (TeX-source-correlate-mode 1)))

(add-hook 'pdf-view-mode-hook 'auto-revert-mode)
(setq auto-revert-interval 0.5)

(add-hook 'pdf-view-mode-hook
          (lambda ()
            (linum-mode -1) ))

(add-hook 'pdf-view-mode-hook
          (lambda ()
            (pdf-view-fit-page-to-window) ))

;; LateX keywords that need colouring
 (setq font-latex-match-reference-keywords
  '(
    ("ac" "[{")
    ("todo" "[{")
    ("kw" "[{")
    ("vsl" "[{")
    ("vpp" "[{")
    ("vrt" "[{")
    ("acp" "[{")
    ("contribution" "[{")
    ("acrodef" "[{")
    ("crefName" "[{")
    ("cref" "[{")
    ("Cref" "[{")
    ("cnref" "[{")
    ("vdmkw" "[{")
))

(add-hook 'LaTeX-mode-hook
          '(lambda ()
             (reftex-mode)))

;;
;; Outline minor mode
;;
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

; Outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")
; HIDE
(define-key cm-map "q" 'hide-sublevels)    ; Hide everything but the top-level headings
(define-key cm-map "t" 'hide-body)         ; Hide everything but headings (all body lines)
(define-key cm-map "o" 'hide-other)        ; Hide other branches
(define-key cm-map "c" 'hide-entry)        ; Hide this entry's body
(define-key cm-map "l" 'hide-leaves)       ; Hide body lines in this entry and sub-entries
(define-key cm-map "d" 'hide-subtree)      ; Hide everything in this entry and sub-entries
; SHOW
(define-key cm-map "a" 'show-all)          ; Show (expand) everything
(define-key cm-map "e" 'show-entry)        ; Show this heading's body
(define-key cm-map "i" 'show-children)     ; Show this heading's immediate child sub-headings
(define-key cm-map "k" 'show-branches)     ; Show all sub-headings under this heading
(define-key cm-map "s" 'show-subtree)      ; Show (expand) everything in this heading & below
; MOVE
(define-key cm-map "u" 'outline-up-heading)                ; Up
(define-key cm-map "n" 'outline-next-visible-heading)      ; Next
(define-key cm-map "p" 'outline-previous-visible-heading)  ; Previous
(define-key cm-map "f" 'outline-forward-same-level)        ; Forward - same level
(define-key cm-map "b" 'outline-backward-same-level)       ; Backward - same level
(global-set-key "\M-o" cm-map)

(provide 'latex-config-pvj)
