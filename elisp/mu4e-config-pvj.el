;;
;; Configuration related to emails
;;
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(require 'org-mu4e)
(require 'mu4e-contrib)

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
           :enter-func (lambda () (mu4e-message "Switch to the Work context"))
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

;; Fetching emails
(setq
  mu4e-get-mail-command "offlineimap"
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
(defvar spam-folders "maildir:/private/[Gmail].Spam OR maildir:/work/\"Junk E-Mail\"")
(defvar draft-folders "maildir:/private/[Gmail].Drafts OR maildir:/work/Drafts")
(defvar not-trash "NOT (maildir:/private/[Gmail].Trash OR maildir:/work/\"Deleted Items\")")
(defvar not-spam (concat "NOT (" spam-folders ")"))
(defvar unread (concat not-spam " AND flag:unread AND " not-trash))

(add-to-list 'mu4e-bookmarks
             '((concat not-spam " AND date:today..now")                  "Today's messages"     ?t))
(add-to-list 'mu4e-bookmarks
             '((concat not-spam " AND date:7d..now")                     "Last 7 days"          ?w))
(add-to-list 'mu4e-bookmarks
             '((concat not-spam " AND mime:image/*")                     "Messages with images" ?p))
(add-to-list 'mu4e-bookmarks
             '(spam-folders "All spambuckets"     ?S))
(add-to-list 'mu4e-bookmarks
             '(draft-folders "All drafts"     ?d))
(add-to-list 'mu4e-bookmarks
             '(unread "Unread messages"      ?u))

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

(provide 'mu4e-config-pvj)
