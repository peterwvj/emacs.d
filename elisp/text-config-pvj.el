
(require 'browse-kill-ring)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;
;; Spell checking
;;
(defconst danish-dictionary "danish" "String used to represent the danish dictionary")
(defconst british-dictionary "british" "String used to represent the british dictionary")

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(setq ispell-dictionary british-dictionary)
(setq ispell-current-dictionary british-dictionary)

(dolist (hook '(text-mode-hook))
 (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Toggle between british/danish dictionaries and input methods. Note
;; that when the input mode is set to danish (i.e. danish-postfix)
;; then 'oe', 'aa' and 'ae' are translated to ø, å and æ,
;; respectively.
(defun pvj/switch-language()
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic british-dictionary)
                     '("danish" . "danish-postfix")
                   '("british" . nil))))
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

;; Inspired by https://github.com/jhaubrich/emacs
(defun pvj/lorem-ipsum ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

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

;; Disable automatic indentation of newlines
(electric-indent-mode -1)

;; Enable smart paring
(electric-pair-mode)

;; Make lambdas appear as λ and so on
(setq prettify-symbols-unprettify-at-point 'right-edge)
(global-prettify-symbols-mode)

(provide 'text-config-pvj)
