;;
;; LaTeX configuration
;;

(use-package tex
  :ensure auctex
  :config

  ;; Indent items by two spaces.
  (setq LaTeX-item-indent 0)
  
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
              (pdf-view-fit-page-to-window) ))

  (add-hook 'LaTeX-mode-hook
            '(lambda ()
               (reftex-mode))))

;;
;; Retrieve BibTeX entries
;;
;; Call 'gscholar-bibtex' to retrieve BibTeX entries from Google
;; Scholar, ACM Digital Library, IEEE Xplore and DBLP.
(use-package gscholar-bibtex)

;;
;; Reformat BibTeX using bibclean
;;
(use-package bibclean-format
  :bind (:map
         bibtex-mode-map
         ("C-c f" . bibclean-format)))

(provide 'latex-config-pvj)
