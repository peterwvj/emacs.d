
(use-package expand-region
  :config
  ;; Assumes that some key (e.g. CAPS LOCK) is mapped to f13.
  ;; On Linux this can be achieve by executing:
  ;; xmodmap -e 'clear Lock' -e 'keycode 66 = F13'
  ;; Inspired by http://sachachua.com/blog/2008/08/emacs-caps-lock-as-m-x/
  (when (and (string-equal system-type "gnu/linux"))
    (progn
      (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = F13'")
      (global-set-key [f13] 'er/expand-region))))

;;
;; Distraction free writing
;;
(use-package writeroom-mode
  :bind
  (([f1] . writeroom-mode))
  :config
  (add-to-list 'writeroom-global-effects
               (lambda (arg)
                 (cond
                  ((= arg 1)
                   (nlinum-mode -1))
                  ((= arg -1)
                   (nlinum-mode 1)))))
  (setq writeroom-fringes-outside-margins nil)
  (setq writeroom-restore-window-config t))

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
(use-package undo-tree :config (global-undo-tree-mode))

;; Move text
(use-package move-text
  :config
  (move-text-default-bindings))

;;
;; Hungry deletion for programming modes
;;
(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

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
  :init (setq markdown-command "multimarkdown"))

;;
;; Check spelling
;;
(require 'flyspell)

;; To improve performance
(setq flyspell-issue-message-flag nil)

(setq ispell-dictionary "en")
(setq ispell-current-dictionary "en")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Duplicate line or region
;; Inspired by http://rejeep.github.io/emacs/elisp/2010/03/11/duplicate-current-line-or-region-in-emacs.html
(defun pvj/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
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
(defun pvj/kill-word-or-region (begining end)
  (interactive "r")
  (if (use-region-p)
      (kill-region begining end)
    (backward-kill-word 1)))

;;
;; Disable auto-copy to clipboard for mouse
;;
(setq mouse-drag-copy-region nil)

(put 'upcase-region 'disabled nil)

;; Use spaces for indentation
(setq-default indent-tabs-mode nil)

;; Setup tab size
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; Indent lines automatically
(electric-indent-mode 1)

;; Enable smart paring
(electric-pair-mode)

(defun pvj/inhibit-electric-pair-mode (char)
  (minibufferp))

(setq electric-pair-inhibit-predicate #'pvj/inhibit-electric-pair-mode)

;; Make lambdas appear as λ and so on
(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode)

(use-package guess-language
  :config
  (setq guess-language-languages '(en da))
  (setq guess-language-min-paragraph-length 35)

  (add-hook 'guess-language-after-detection-functions
            (lambda (lang beginning end)
              (if (string= lang "da")
                  (set-input-method "danish-postfix")
                (set-input-method nil))))

  (add-hook 'text-mode-hook (lambda () (guess-language-mode 1))))

;;
;; For language analysis
;;
(use-package langtool
  :config
  (setq langtool-language-tool-jar "~/tools/LanguageTool-3.4/languagetool-commandline.jar"))

(provide 'text-config-pvj)
