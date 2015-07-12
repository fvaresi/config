;; export org files to confluence
;;(require 'ox-confluence)

(setq standup-journal "~/autocomm/docs/standup-journal.org")

(setq org-agenda-files `(,standup-journal))

;; org capture
(setq org-directory "~/org/")
(setq org-mobile-directory "~/Dropbox/org-mobile")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-capture-templates
      `(("s" "Standup" entry
	 (file+datetree+prompt ,standup-journal)
	 "* %T - %?" :kill-buffer t)))

(provide 'setup-org)
