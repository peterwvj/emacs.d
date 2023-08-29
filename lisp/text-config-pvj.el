
(use-package expand-region
  :bind
  (("C-c e" . er/expand-region))
  :config
  (add-hook 'text-mode-hook
            (lambda ()
              (append
               er/try-expand-list
               '(mark-paragraph
                 mark-page)))))

;;
;; Distraction free writing
;;
(use-package writeroom-mode
  :config
  (setq writeroom-fringes-outside-margins nil)
  (setq writeroom-restore-window-config t))

(use-package zop-to-char
  :config
  (global-set-key [remap zap-to-char] 'zop-to-char)
  (global-set-key "\M-Z" 'zop-up-to-char))

;;
;; Display form feed characters as tidy horizontal lines
;;
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

;;
;; Multiple cursors
;;
(use-package multiple-cursors)

;;
;; Enable undo-tree
;;
(use-package undo-tree
  :pin "gnu"
  :config
  (progn
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
    (global-undo-tree-mode)))

;; Move text
(use-package move-text
  :config
  (move-text-default-bindings))

;;
;; Hungry deletion for programming modes
;;
(use-package hungry-delete
  :config
  (progn
    (setq hungry-delete-chars-to-skip " \t\r\f\v")
    (global-hungry-delete-mode)))

;;
;; Go to last change
;;
(use-package goto-chg)

;;
;; Look up the definition of a word
;;
(use-package define-word)

;;
;; Visual feedback on text operations
;;
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))


;;
;; Revert the buffer when changes are detected
;;
(require 'autorevert)
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;;
;; Markdown
;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (bind-key "<backspace>" #'hungry-delete-backward markdown-mode-map))

;;
;; Check spelling
;;
(require 'flyspell)

;; To improve performance
(setq flyspell-issue-message-flag nil)

(setq ispell-dictionary "en")
(setq ispell-current-dictionary "en")

(add-hook 'text-mode-hook (lambda ()
                            (shut-up
                              (flyspell-mode))))

(add-hook 'prog-mode-hook (lambda ()
                            (shut-up
                              (flyspell-prog-mode))))

;; Duplicate line or region
;; Inspired by http://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun pvj/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; Find repeated word, e.g. 'the the'
;; Inspired by https://www.gnu.org/software/emacs/manual/html_node/eintr/the_002dthe.html
(defun pvj/find-repeated-words ()
  "Search forward for for a duplicated word."
  (interactive)
  (message "Searching for for duplicated words ...")
  (push-mark)
  ;; This regexp is not perfect
  ;; but is fairly good over all:
  (if (re-search-forward
       "\\b\\([^@ \n\t]+\\)[ \n\t]+\\1\\b" nil 'move)
      (message "Found duplicated word.")
    (message "End of buffer")))

;; Insert dummy pseudo Latin text
(use-package lorem-ipsum)

;; Can be used to overload C-w so if no region is selected then the
;; last word is deleted.
(defun pvj/kill-word-or-region (beginning end)
  "Kill region, if selected, otherwise kill word.
Argument BEGINNING start of region
Argument END end of region."
  (interactive "r")
  (if (use-region-p)
      (kill-region beginning end)
    (backward-kill-word 1)))

;; Yank without formatting
(setq yank-excluded-properties t)

;; Disable auto-copy to clipboard for mouse
(setq mouse-drag-copy-region nil)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Setup tab size
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(electric-indent-mode 1)
;; Disable electric-indent-mode for certain major modes.
(mapc
 (lambda (language-mode-hook)
   (add-hook language-mode-hook
             (lambda ()
               (electric-indent-local-mode -1))))
 '(text-mode-hook
   bibtex-mode-hook))

;; Inspired by https://stackoverflow.com/questions/22107501/set-emacs-to-smart-auto-line-after-a-parentheses-pair/22109370#22109370
(defun pvj/newline-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
                             (and (looking-back ">") (looking-at "<"))
                             (and (looking-back "(") (looking-at ")"))
                             (and (looking-back "\\[") (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

(global-set-key (kbd "M-RET") 'pvj/newline-dwim)

;; Enable smart pairing
(electric-pair-mode 1)

;; Make lambdas appear as λ and so on
(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode)

(use-package guess-language
  :config
  (setq guess-language-languages '(en da))
  (setq guess-language-min-paragraph-length 35)
  (setq guess-language-langcodes '((da "dansk" nil)
                                   (en "en_GB" "English")))

  (add-hook 'text-mode-hook (lambda () (guess-language-mode 1))))

;;
;; For language analysis
;;
(use-package langtool
  :config
  (setq langtool-language-tool-jar "~/tools/LanguageTool-3.4/languagetool-commandline.jar"))

(use-package comment-dwim-2
  :bind
  (("M-;" . comment-dwim-2)))

(defun pvj/smart-kill-whole-line (&optional arg)
  "Wrapper for the function `kill-whole-line' that respects indentation.
Argument ARG the argument passed to 'kill-whole-line'."
  (interactive "P")
  (kill-whole-line arg)
  (back-to-indentation))

(global-set-key (kbd "C-c q") 'pvj/duplicate-current-line-or-region)
(delete-selection-mode 1)
(global-set-key (kbd "C-w") 'pvj/kill-word-or-region)
(global-set-key (kbd "<f8>")
                (lambda ()
                  (interactive)
                  (if (null current-input-method)
                      (set-input-method "danish-postfix")
                    (set-input-method nil))))
(global-set-key (kbd "M-Q")   'pvj/unfill-paragraph)
(global-set-key (kbd "<f9>") 'revert-buffer)

;; Writable grep buffer
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(provide 'text-config-pvj)
