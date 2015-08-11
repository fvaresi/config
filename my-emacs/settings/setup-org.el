;; export org files to confluence
;;(require 'ox-confluence)

(setq standup-journal "~/autocomm/docs/standup-journal.org")

(setq org-agenda-files `(,standup-journal))
(setq org-agenda-include-diary t)

;; org capture
(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/org-mobile")
(setq org-default-notes-file (concat org-directory "/notes.org"))

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
(setq org-mobile-files `(org-agenda-files ,org-default-notes-file))
(setq org-mobile-inbox-for-pull "~/org/from-mobile.org")

;; org todo
(setq org-todo-keywords `((sequence "TODO(t)" "IN_PROGRESS(p)" "|" "DONE(d)")))

(provide 'setup-org)
