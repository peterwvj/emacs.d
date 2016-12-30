;;
;; Configuration related to emails
;;

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :ensure f
  :config 
  (progn
    (use-package org-mu4e
      :ensure f)
    (use-package mu4e-contrib
      :ensure f)
    (use-package mu4e-maildirs-extension
      :ensure f)))

;;
;; To prevent issue with long email conversations - see
;; https://github.com/djcb/mu/issues/919
;;
(setq max-specpdl-size 2600)  

(when (fboundp 'imagemagick-register-types)
        (imagemagick-register-types))


;; Do not show mu4e index messages
(setq mu4e-hide-index-messages t)

;; Do not highlight current line in main view and view mode
(add-hook 'mu4e-main-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

;; Disable line numbers
(add-hook 'mu4e-main-mode-hook (lambda ()
                                 (linum-mode -1)))

(add-hook 'mu4e-view-mode-hook (lambda ()
                            (setq-local global-hl-line-mode nil)))

(setq mu4e-maildir "~/Maildir")

;; Setup environment for main account
(setq mu4e-sent-folder "/private/[Gmail].Sent Mail"
      mu4e-drafts-folder "/private/[Gmail].Drafts"
      mu4e-trash-folder "/private/[Gmail].Trash"
      user-mail-address "peter.w.v.jorgensen@gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 587)

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Private"
           ;; :enter-func (lambda () (mu4e-message "Switch to the Private context"))
           ;; leave-func not defined
           :match-func (lambda (msg)
                         (when msg 
                           (mu4e-message-contact-field-matches msg 
                                                               :to "peter.w.v.jorgensen@gmail.com")))
           :vars '(  ( user-mail-address	     . "peter.w.v.jorgensen@gmail.com"  )
                     ( user-full-name	    . "Peter Würtz Vinther Tran-Jørgensen" )
                     (mu4e-sent-folder . "/private/[Gmail].Sent Mail")
                     (mu4e-drafts-folder . "/private/[Gmail].Drafts")
                     (mu4e-trash-folder . "/private/[Gmail].Trash")
                     (smtpmail-default-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-service . 587)))
         ,(make-mu4e-context
           :name "Work"
           ;; :enter-func (lambda () (mu4e-message "Switch to the Work context"))
           ;; leave-fun not defined
           :match-func (lambda (msg)
                         (when msg 
                           (mu4e-message-contact-field-matches msg 
                                                               :to "pvj@eng.au.dk")))
           :vars '(  ( user-mail-address	     . "pvj@eng.au.dk" )
                     ( user-full-name	    . "Peter Würtz Vinther Tran-Jørgensen" )
                     (mu4e-sent-folder . "/work/Sent Items")
                     (mu4e-drafts-folder . "/work/Drafts")
                     (mu4e-trash-folder . "/work/Deleted Items")
                     (smtpmail-default-smtp-server . "asmtp.au.dk")
                     (smtpmail-smtp-server . "asmtp.au.dk")
                     (smtpmail-smtp-service . 587)))))

;; Set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
;; guess or ask the correct context, e.g.

;; Start with the first (default) context; 
;; default is to ask-if-none (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask 
;; '(setq mu4e-compose-context-policy nil)

(setq mu4e-headers-date-format "%d/%b/%Y %H:%M")

;; a  list of user's e-mail addresses
(setq mu4e-user-mail-address-list '("peter.w.v.jorgensen@gmail.com" "pvj@eng.au.dk"))

;; header fields
(setq mu4e-headers-fields
      '( (:date       . 18)
         (:flags   . 6)
         (:from-or-to . 22) ;; Determined using mu4e-user-mail-address-list
         (:subject    . nil)))

;; Don't save message to Sent Messages, GMail/IMAP will take care of this
;; (setq mu4e-sent-messages-behavior 'delete)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Setup some handy shortcuts
(setq mu4e-maildir-shortcuts
      '(("/private/INBOX"             . ?i)
        ("/private/[Gmail].Sent Mail" . ?s)
        ("/private/[Gmail].Smuk"     . ?m)
        ("/work/INBOX"     . ?k)
        ("/work/Sent Items"     . ?x)
        ("/work/Overture"     . ?d)
        ("/work/PhD.Projects.CGen"     . ?c)
        ))

;; a V opens the current message in the default web browsers.
(add-to-list 'mu4e-view-actions 
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; a x opens with xwidget
(add-to-list 'mu4e-view-actions 
             '("xViewXWidget" . mu4e-action-view-with-xwidget) t) 

(setq mu4e-view-show-images t)
(setq mu4e-html2text-command 'mu4e-shr2text)
;; When using a dark theme the messages are difficult to read
(setq shr-color-visible-luminance-min 80)
(setq mu4e-view-prefer-html t)
(setq mu4e-use-fancy-chars t)

;;; Save attachment (this can also be a function)
(setq mu4e-attachment-dir "~/Downloads")

(define-key mu4e-main-mode-map "q" 'quit-window)
(define-key mu4e-main-mode-map "Q" 'mu4e-quit)
(define-key mu4e-view-mode-map (kbd "C-n") 'org-next-link)
(define-key mu4e-view-mode-map (kbd "C-p") 'org-previous-link)
(define-key mu4e-view-mode-map [home] 'beginning-of-visual-line)
(define-key mu4e-view-mode-map [end] 'end-of-visual-line)

;; Fetching emails.
;;
;; The -q argument is used to run a quick synchronisations. From the
;; offlineimap manual: "Ignore any flag updates on IMAP servers. If a
;; flag on the remote IMAP changes, and we have the message locally,
;; it will be left untouched in a quick run. This option is ignored if
;; maxage is set".
(setq
  mu4e-get-mail-command "offlineimap -q"
  mu4e-update-interval (* 5 60))

;; Spell-check emails
;; (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

;;; Show Smileys
;;
(add-hook 'mu4e-view-mode-hook 'smiley-buffer)

;; Empty the initial bookmark list
(setq mu4e-bookmarks '())

;; Re-define all standard bookmarks to not include the spam folders
;; for searches - inspired by
;; https://github.com/munen/emacs.d/blob/master/mu4e-config.el

;;
;; 'unread' bookmark:
;;
;; A private mail is considered 'unread' only if it is contained in
;; the private INBOX folder. The reason for not considering any of the
;; other Gmail folders is that Gmail tend to mark mails as 'unread'
;; after they have been marked as 'read' and moved from the INBOX to
;; some other folder.
;;
;; A work related mail is considered 'unread' if it is flagged
;; accordingly.
;;
;; Since I have configured imapfilter to flag spam as 'read' (or seen)
;; these mails are not considered in this bookmark (as they will never
;; be 'unread' anyway)
;;
(defvar unread "flag:unread AND (maildir:/private/INBOX OR maildir:/work/*)")
(defvar spam-folders "maildir:/private/[Gmail].Spam OR maildir:/work/\"Junk E-Mail\"")
(defvar not-spam (concat "NOT (" spam-folders ")"))
(defvar draft-folders "maildir:/private/[Gmail].Drafts OR maildir:/work/Drafts")

(add-to-list 'mu4e-bookmarks
             '((concat not-spam " AND mime:image/*") "Messages with images" ?p))
(add-to-list 'mu4e-bookmarks
             '(spam-folders "All spambuckets" ?S))
(add-to-list 'mu4e-bookmarks
             '(draft-folders "All drafts" ?d))
(add-to-list 'mu4e-bookmarks
             '(unread "Unread messages" ?u))

;; Email notifications
(mu4e-alert-set-default-style 'libnotify)
;; (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
;; (setq mu4e-alert-notify-repeated-mails t)
(setq mu4e-alert-interesting-mail-query unread)

;; C-c C-a	` attach a file (pro-tip: drag & drop works as well)

;; Check for supposed attachments prior to sending an email
;; Inspired by https://github.com/munen/emacs.d
(defvar attachment-regexp "\\([Ww]e send\\|[Ii] send\\|[Jj]eg sender\\|[Vv]i sender\\|[Aa]ttach\\|[Vv]edhæft\\)")
(defun check-for-attachment nil
  "Check if there is an attachment in the message if I claim it."
  (save-excursion
    (message-goto-body)
    (when (search-forward-regexp attachment-regexp nil t nil)
      (message-goto-body)
      (unless (or (search-forward "<#part" nil t nil)
                  (message-y-or-n-p
                   "No attachment. Send the message ?" nil nil))
        (error "No message sent")))))

(add-hook 'message-send-hook 'check-for-attachment)


;; Disable "kill buffer with xwidgets" question
(remove-hook 'kill-buffer-query-functions 'xwidget-kill-buffer-query-function)

;; Add a maildir summary to the mu4e-main-view
(defun pvj/mu4e-maildirs-extension-propertize-unread-only (item)
  "Propertize only the maildir unread count using ITEM plist."
  (let ((unread (or (plist-get item :unread) 0))
        (total (or (plist-get item :total) 0))
        (name (plist-get item :name)))
    (concat (format "\t%s%s %s "
                    (plist-get item :indent)
                    (plist-get item :prefix)
                    name)
            (if (or (string-match-p "work" name) (string-match-p "private" name))
                (concat "(" (number-to-string total) ")")
              (format "(%s/%s)"
                      (propertize (number-to-string unread)
                                  'face (cond
                                         ((> unread 0) 'mu4e-maildirs-extension-maildir-hl-face)
                                         (t            'mu4e-maildirs-extension-maildir-face)))
                      total)))))

(setq mu4e-maildirs-extension-propertize-func 'pvj/mu4e-maildirs-extension-propertize-unread-only)

(setq mu4e-maildirs-extension-custom-list '("/private/INBOX" "private/[Gmail].Announcements" "/work/INBOX" "/work/Overture" "/work/JML" "/work/PhD.Announcements"))

(mu4e-maildirs-extension)

(provide 'mu4e-config-pvj)
