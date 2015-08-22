;; export org files to confluence
;;(require 'ox-confluence)

(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/org-mobile")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq standup-journal "~/autocomm/docs/standup-journal.org")

(setq org-agenda-files `(,standup-journal ,org-default-notes-file))
(setq org-agenda-include-diary t)

;; org capture
(setq org-capture-templates
      `(("t" "Todo" entry
	 (file+headline org-default-notes-file "Tasks")
	 "* TODO %?"
	 :kill-buffer t)

	("b" "Bookmark" entry
	 (file+headline org-default-notes-file "Bookmarks")
	 "* %?\n:CREATED: %U\n:END:\n\n"
	 :empty-lines 1)

	("s" "Standup" entry
	 (file+datetree+prompt ,standup-journal)
	 "* %T - %?"
	 :kill-buffer t)))

;; org clock
(setq org-clock-out-remove-zero-time-clocks t)

;; org log
(setq org-log-into-drawer t)

;; org mobile
(setq org-mobile-files `(org-agenda-files ,org-default-notes-file "~/autocomm/docs/sprints.org"))
(setq org-mobile-inbox-for-pull "~/org/from-mobile.org")

(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs)
  (when org-mobile-push-timer
    (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

(add-hook 'after-save-hook 
 (lambda () 
   (when (eq major-mode 'org-mode)
     (dolist (file (org-mobile-files-alist))
      (if (string= (file-truename (expand-file-name (car file)))
		   (file-truename (buffer-file-name)))
           (org-mobile-push-with-delay 30)))
   )))

;; org notmuch
(require 'org-notmuch)

;; org todo
(setq org-todo-keywords `((sequence "TODO(t)" "IN_PROGRESS(p)" "|" "DONE(d)")))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(provide 'setup-org)
