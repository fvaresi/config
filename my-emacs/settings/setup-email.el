;; Use msmtp for sending mails
(setq sendmail-program "/usr/bin/msmtp")
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-fill-column nil)
(setq mail-specify-envelope-from t
      ;; needed for debians message.el cf. README.Debian.gz
      message-sendmail-f-is-evil nil
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)

;; Address completion
(require 'notmuch-address)
(setq notmuch-address-command "/home/fvaresi/bin/goobook-notmuch")
(notmuch-address-message-insinuate)

(require 'notmuch)
(setq notmuch-always-prompt-for-sender t)
(setq notmuch-crypto-process-mime t)
(setq notmuch-search-oldest-first nil)
(setq message-kill-buffer-on-exit t)

;; Sign messages by default.
;; (add-hook 'message-setup-hook 'mml-secure-message-sign)

;; this gives preference to text/html over text/plain
;;(setq notmuch-multipart/alternative-discouraged '("text/plain" "text/html"))

(defun search-toggle-message-delete ()
  "toggle deleted tag for message"
  (interactive)
  (notmuch-search-tag
   (list
    (if (member "deleted" (notmuch-search-get-tags))
	"-deleted" "+deleted"))))

(defun search-toggle-delete-all()
  "toggle deleted tag for all message"
  (interactive)
  (notmuch-search-tag-all
   (list
    (if (member "deleted" (notmuch-search-get-tags))
	"-deleted" "+deleted"))))

(defun show-toggle-message-delete ()
  "toggle deleted tag for message"
  (interactive)
  (notmuch-show-tag
   (list
    (if (member "deleted" (notmuch-show-get-tags))
	"-deleted" "+deleted"))))

(defun show-toggle-thread-delete ()
  "toggle deleted tag for message"
  (interactive)
  (notmuch-show-tag-all
   (list
    (if (member "deleted" (notmuch-show-get-tags))
	"-deleted" "+deleted"))))

(defun tree-toggle-message-delete ()
  "toggle deleted tag for message"
  (interactive)
  (notmuch-tree-tag
   (list
    (if (member "deleted" (notmuch-tree-get-tags))
	"-deleted" "+deleted"))))

(defun tree-toggle-thread-delete ()
  "toggle deleted tag for thread"
  (interactive)
  (notmuch-tree-tag-thread
   (list
    (if (member "deleted" (notmuch-tree-get-tags))
	"-deleted" "+deleted"))))

(defun search-toggle-message-fav ()
  "toggle deleted tag for message"
  (interactive)
  (notmuch-search-tag
   (list
    (if (member "flagged" (notmuch-search-get-tags))
	"-flagged" "+flagged"))))

(defun show-toggle-message-fav ()
  "toggle deleted tag for message"
  (interactive)
  (notmuch-show-tag
   (list
    (if (member "flagged" (notmuch-show-get-tags))
	"-flagged" "+flagged"))))

(defun tree-toggle-message-fav ()
  "toggle deleted tag for message"
  (interactive)
  (notmuch-tree-tag
   (list
    (if (member "flagged" (notmuch-tree-get-tags))
	"-flagged" "+flagged"))))

(defun search-toggle-message-inbox ()
  "toggle inbox tag for message"
  (interactive)
  (notmuch-search-tag
   (list
    (if (member "inbox" (notmuch-search-get-tags))
	"-inbox" "+inbox"))))

(defun show-toggle-message-inbox ()
  "toggle inbox tag for message"
  (interactive)
  (notmuch-show-tag
   (list
    (if (member "inbox" (notmuch-show-get-tags))
	"-inbox" "+inbox"))))

(defun tree-toggle-thread-inbox ()
  "toggle inbox tag for thread"
  (interactive)
  (notmuch-tree-tag-thread
   (list
    (if (member "inbox" (notmuch-tree-get-tags))
	"-inbox" "+inbox"))))

(defun show-toggle-message-unread ()
  "toggle unread tag for message"
  (interactive)
  (notmuch-show-tag
   (list
    (if (member "unread" (notmuch-show-get-tags))
	"-unread" "+unread"))))

(defun search-toggle-message-unread ()
  "toggle unread tag for message"
  (interactive)
  (notmuch-search-tag
   (list
    (if (member "unread" (notmuch-search-get-tags))
	"-unread" "+unread"))))

(defun reply-to-thread-show ()
  (interactive)
  (notmuch-show-reply 't))

(defun reply-to-thread-sender-show ()
  (interactive)
  (notmuch-show-reply-sender 't))

(defun reply-to-thread-search ()
  (interactive)
  (notmuch-search-reply-to-thread 't))

(defun reply-to-thread-sender-search ()
  (interactive)
  (notmuch-search-reply-to-thread-sender 't))

(defun switch-to-mail-persp ()
  (interactive)
  (persp-switch "mail")
  (notmuch-search-unread))

(defun notmuch-search-unread ()
  (interactive)
  (notmuch-search "tag:inbox"))

(defun notmuch-jump-to-tag ()
  (interactive)
  (let ((selected-tag (helm :sources `((name . "Tags")
				       (candidates . ,(notmuch-tag-completions))
				       (pattern-transformer . (lambda (pattern) (regexp-quote pattern)))
				       (action . identity))
			    :buffer "*email tags*"
			    :keymap helm-buffer-map)))
    (notmuch-search (concat "tag:" selected-tag))))

;; (defun show-email-externally ()
;;   (interactive)
;;   (notmuch-show-pipe-message nil "view-html.sh"))

;; (defun show-email-externally-full-thread ()
;;   (interactive)
;;   (notmuch-show-pipe-message 't "view-html.sh"))

(provide 'setup-email)
