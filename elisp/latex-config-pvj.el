;;
;; LaTeX configuration
;;

(use-package tex
  :ensure auctex
  :config

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

  (setq TeX-auto-save t
        TeX-parse-self t)

  ;; Needed to sync TeX and PDF
  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (TeX-source-correlate-mode 1)))

  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

  ;; (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
  ;; (setq auto-revert-interval 0.5)

  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (nlinum-mode -1) ))

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
               (reftex-mode))))

;;
;; Retrieve BibTeX entries
;;
;; Call 'gscholar-bibtex' to retrieve BibTeX entries from Google
;; Scholar, ACM Digital Library, IEEE Xplore and DBLP.
(use-package gscholar-bibtex)

(provide 'latex-config-pvj)
