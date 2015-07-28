;; export org files to confluence
;;(require 'ox-confluence)

(setq standup-journal "~/autocomm/docs/standup-journal.org")

(setq org-agenda-files `("~/autocomm/docs/standup.org" ,standup-journal))

;; org capture
(setq org-directory "~/org/")
(setq org-mobile-directory "~/Dropbox/org-mobile")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates
      `(("t" "Todo" entry
	 (file+headline org-default-notes-file "Tasks")
	 "* TODO %?"
	 :kill-buffer t)

	("b" "Bookmark" entry
	 (file+headline org-default-notes-file) "Bookmarks"
	 "* %?\n:CREATED: %U\n:END:\n\n"
	 :empty-lines 1)

	("s" "Standup" entry
	 (file+datetree+prompt ,standup-journal)
	 "* %T - %?"
	 :kill-buffer t)))

(provide 'setup-org)
