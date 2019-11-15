;;
;; Configuration related to emails
;;

(defvar pvj/mu4e-dir "/usr/local/share/emacs/site-lisp/mu4e")

(use-package mu4e
  :if (and
       (eq system-type 'gnu/linux)
       (null noninteractive)
       (file-directory-p pvj/mu4e-dir))
  :init
  (defun pvj/remove-signature ()
    "Remove signature from message."
    (interactive)
    (save-excursion
      (message-goto-signature)
      (forward-line -1)
      (delete-region (point) (point-max))))
  
  (defun pvj/mu4e-browse-if-url ()
    (interactive)
    (let ((url (shr-url-at-point nil)))
      (if url
          (browse-url url)
        ;; Fall back on default behaviour
        (mu4e-scroll-up))))

  (defun pvj/update-inboxes ()
    "Quickly update inboxes for private and work accounts."
    (interactive)
    (let ((mu4e-get-mail-command "offlineimap -q -f INBOX"))
      (mu4e-update-mail-and-index nil)))

  (defun pvj/insert-detailed-work-signature ()
    "Insert detailed work signature in the current message."
    (interactive)
    (progn
      (pvj/remove-signature)
      (let ((message-signature (concat "Best regards,\n"
                                       "Peter W. V. Tran-Jørgensen\n"
                                       "Postdoc, Software Engineering\n"
                                       "Aarhus University, Department of Engineering\n"
                                       "Finlandsgade 22, DK-8200, Aarhus N")))
        (message-insert-signature))))
  
  :load-path pvj/mu4e-dir
  :bind (("<f2>" . mu4e))
  :ensure f
  :config
  (use-package org-mu4e
    :ensure f)
  (use-package mu4e-contrib
    :ensure f)

  (bind-keys :map mu4e-view-mode-map
             ("<return>" . pvj/mu4e-browse-if-url))
  
  ;;
  ;; To prevent issue with long email conversations - see
  ;; https://github.com/djcb/mu/issues/919
  ;;
  (setq max-specpdl-size 32000)

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))


  ;; Do not show mu4e index messages
  (setq mu4e-hide-index-messages t)

  ;; Do not highlight current line in main view and view mode
  (add-hook 'mu4e-main-mode-hook (lambda ()
                                   (setq-local global-hl-line-mode nil)))

  ;; Turn off auto-fill-mode in the mu4e message buffer
  (add-hook 'mu4e-compose-mode-hook
            (lambda ()
              (auto-fill-mode -1)))

  ;; Use ivy completion but ignore the initial input
  (defun pvj/mu4e-completing-read (prompt collection &optional predicate require-match
                                          initial-input hist def inherit-input-method)
    (ivy-completing-read prompt collection predicate require-match nil hist def inherit-input-method))

  ;; Use different completion method and ignore the initial completion
  ;; input
  (setq mu4e-completing-read-function 'pvj/mu4e-completing-read)

  ;; Format the reply-quote-string
  (setq message-citation-line-format "On %a %d %b %Y at %R, %f wrote:\n")
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  
  (setq mu4e-maildir "~/Maildir")

  (setq mu4e-compose-signature "Best regards,\nPeter")
  
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

  (setq mu4e-context-policy 'pick-first)

  (setq mu4e-headers-include-related nil)

  (setq mu4e-headers-date-format "%d/%b/%y %H:%M")

  ;; Set the number of lines to be shown in the headers view when it
  ;; is split
  ;; (setq mu4e-headers-visible-lines 18)

  ;; Don't split the headers view when viewing messages
  ;; (setq mu4e-split-view nil)
  
  ;; A list of user's e-mail addresses
  (setq mu4e-user-mail-address-list '("peter.w.v.jorgensen@gmail.com" "pvj@eng.au.dk"))
  
  (setq mu4e-headers-fields
        '((:date . 16)
          (:flags . 6)
          (:from-or-to . 25) ;; Determined using mu4e-user-mail-address-list
          (:maildir . 25)
          (:subject . nil)))
  
  (add-hook 'mu4e-headers-mode-hook (lambda ()
                                      (visual-line-mode -1)
                                      (toggle-truncate-lines 1)))
  
  ;; Don't save message to Sent Messages, GMail/IMAP will take care of this
  ;; (setq mu4e-sent-messages-behavior 'delete)

  ;; Don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Show the email in the address field (in addition to the name)
  (setq mu4e-view-show-addresses t)

  ;; Setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/private/INBOX"             . ?i)
          ("/private/[Gmail].Sent Mail" . ?s)
          ("/work/INBOX"     . ?k)
          ("/work/Sent Items"     . ?x)))

  ;; a V opens the current message in the default web browser.
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; a x opens the current message using xwidget
  (add-to-list 'mu4e-view-actions
               '("xViewXWidget" . mu4e-action-view-with-xwidget) t)

  (setq mu4e-view-show-images t)
  (setq mu4e-view-prefer-html t)
  ;; Use of fancy characters may introduce quite a performance
  ;; hit. See https://github.com/djcb/mu/issues/163
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-headers-results-limit 100)

  ;; Make it easier to read messages when using a dark theme
  (setq shr-color-visible-luminance-min 80)

  ;; To convert messages to PDF
  (setq mu4e-msg2pdf "/usr/bin/msg2pdf")
  
  ;; Save attachment (this can also be a function)
  (setq mu4e-attachment-dir "~/Downloads")

  ;; Use gnus article view
  (setq mu4e-view-use-gnus t)

  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (setq mu4e-icalendar-trash-after-reply t)
  
  (define-key mu4e-main-mode-map "q" 'quit-window)
  (define-key mu4e-main-mode-map "Q" 'mu4e-quit)
  (define-key mu4e-view-mode-map (kbd "C-n") 'org-next-link)
  (define-key mu4e-view-mode-map (kbd "C-p") 'org-previous-link)
  (define-key mu4e-view-mode-map [home] 'beginning-of-visual-line)
  (define-key mu4e-view-mode-map [end] 'end-of-visual-line)

  ;; Fetching emails.
  (setq
   mu4e-get-mail-command "offlineimap -q"
   mu4e-update-interval (* 1 60))

  ;; Show Smileys
  ;; (add-hook 'mu4e-view-mode-hook 'smiley-buffer)

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
  ;; appear as 'unread' anyway)
  ;;
  (defvar spam-folders "maildir:/private/[Gmail].Spam OR maildir:/work/\"Junk E-Mail\"")
  (defvar not-spam (concat "NOT (" spam-folders ")"))
  (defvar unread (concat "flag:unread AND (maildir:/private/INBOX OR maildir:/work/*)" "AND " not-spam))
  (defvar draft-folders "maildir:/private/[Gmail].Drafts OR maildir:/work/Drafts")


  (add-to-list 'mu4e-bookmarks
               '((concat not-spam " AND date:today..now") "Today's messages" ?t))
  (add-to-list 'mu4e-bookmarks
               '((concat not-spam " AND date:7d..now") "Last 7 days" ?w))
  (add-to-list 'mu4e-bookmarks
               '((concat not-spam " AND mime:image/*") "Messages with images" ?p))
  (add-to-list 'mu4e-bookmarks
               '(spam-folders "All spambuckets" ?S))
  (add-to-list 'mu4e-bookmarks
               '(draft-folders "All drafts" ?d))
  (add-to-list 'mu4e-bookmarks
               '(unread "Unread messages" ?u))

  ;; Email notifications
  ;; (use-package mu4e-alert
  ;;   :init
  ;;   ;; (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
  ;;   (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  ;;   :config
  ;;   (mu4e-alert-set-default-style 'libnotify)
  ;;   ;; (setq mu4e-alert-notify-repeated-mails t)
  ;;   (setq mu4e-alert-interesting-mail-query unread))

  ;; Single file attachment:
  ;;
  ;; C-c C-a ` attach a file (pro-tip: drag & drop works as well)

  ;; Attach multiple files:
  ;;
  ;; Inspired by
  ;; http://www.djcbsoftware.nl/code/mu/mu4e/Attaching-files-with-dired.html
  ;; Mark the file(s) in dired and press C-c RET C-a.
  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

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

  (setq mu4e-maildirs-extension-custom-list '("/private/INBOX"  "/work/INBOX" "/work/Overture"))

  (use-package mu4e-maildirs-extension
    :config
    (mu4e-maildirs-extension))

  ;; Make sure the gnutls command-line utils are installed, package
  ;; 'gnutls-bin' in Debian/Ubuntu.
  (use-package smtpmail)

  ;; Use same authinfo file for work and private emails
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
        smtpmail-debug-info t)

  (use-package helm-mu
    :bind
    (("C-c m" . helm-mu-contacts))
    :config
    (define-key mu4e-main-mode-map "s" 'helm-mu)
    (define-key mu4e-headers-mode-map "s" 'helm-mu)
    (define-key mu4e-view-mode-map "s" 'helm-mu))

  ;; Start mu4e
  ;; (add-hook 'after-init-hook
  ;;           (lambda () (mu4e)))
  )

(provide 'mu4e-config-pvj)
