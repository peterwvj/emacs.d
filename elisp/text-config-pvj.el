
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
;; Display form feed characters as tidy horizontal lines
;;
(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

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
;; Spell checking
;;
(defconst danish-dictionary "danish" "String used to represent the danish dictionary")
(defconst british-dictionary "british" "String used to represent the british dictionary")

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq ispell-dictionary british-dictionary)
(setq ispell-current-dictionary british-dictionary)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Toggle between british/danish dictionaries and input methods. Note
;; that when the input mode is set to danish (i.e. danish-postfix)
;; then 'oe', 'aa' and 'ae' are translated to ø, å and æ,
;; respectively.
(defun pvj/switch-language()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic british-dictionary)
                     `(,danish-dictionary . "danish-postfix")
                   `(,british-dictionary . nil))))
    (ispell-change-dictionary (car change))
    (set-input-method (cdr change))
    (message "Dictionary switched from %s to %s" dic change)))

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

(provide 'text-config-pvj)
