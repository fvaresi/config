;;;;;;;;;;;;;;;
;; Org Babel ;;
;;;;;;;;;;;;;;;

(require 'ob-http)
(require 'ob-shell)
(setq org-babel-load-languages '((emacs-lisp . t)
				 (http . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate nil)

;; It's possible to define default headers per-language/buffer if required.
(setq org-babel-default-header-args
           (cons '(:exports . "none")
                 (assq-delete-all :exports org-babel-default-header-args)))

(require 'org-bullets)

;; export org files to confluence
;;(require 'ox-confluence)

(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/org-mobile")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq default-journal-file (concat org-directory "/journal.org"))

(setq org-agenda-files `(,org-default-notes-file ,default-journal-file "~/autocomm/docs/sprints.org" "~/life/docs/life.org" "~/infuy/docs/infuy.org"))
(setq org-agenda-include-diary t)

;; org capture
(setq org-capture-templates
      `(("t" "Todo" entry
	 (file+headline org-default-notes-file "Tasks")
	 "* TODO %?"
	 :kill-buffer t)

	("b" "Bookmark" entry
	 (file+headline org-default-notes-file "Bookmarks")
	 "* %c\n:CREATED: %U\n:END:\n\n"
	 :empty-lines 1)

	("e" "Event" entry
	 (file+datetree+prompt ,default-journal-file)
	 "* %^T - %?"
	 :kill-buffer t)
	))
(setq org-refile-targets `((org-agenda-files :maxlevel . 3)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;;;;;;;;;;;;;;
;; Org Clock ;;
;;;;;;;;;;;;;;;

(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-modeline-total 'today)

(setq org-html-validation-link nil)

;;;;;;;;;;;;;;
;; Org Jira ;;
;;;;;;;;;;;;;;

(require 'org-jira)
;;(setq org-jira-serv-alist `(("Autocomm" (:url "http://jira.internetbrands.com/rpc/soap/jirasoapservice-v2?wsdl" :user "fvaresi" :host "http://jira.internetbrands.com"))))
(setq org-jira-use-status-as-todo t)

;; org log
(setq org-log-into-drawer nil)
(setq org-log-state-notes-insert-after-drawers t)

;;;;;;;;;;;;;;;;
;; Org Mobile ;;
;;;;;;;;;;;;;;;;

(setq org-mobile-files `(org-agenda-files))
(setq org-mobile-inbox-for-pull "~/org/from-mobile.org")

(defvar fvaresi/org-mobile-push-timer nil
  "Timer that `fvaresi/org-mobile-push-timer' used to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs)
  (when fvaresi/org-mobile-push-timer
    (cancel-timer fvaresi/org-mobile-push-timer))
  (setq fvaresi/org-mobile-push-timer
        (run-with-idle-timer
         (* 1 secs) nil 'org-mobile-push)))

(defun fvaresi/org-mobile-autopush ()
    (when (eq major-mode 'org-mode)
      (dolist (file (org-mobile-files-alist))
	(if (string= (file-truename (expand-file-name (car file)))
		     (file-truename (buffer-file-name)))
	    (org-mobile-push-with-delay 30)))))

(add-hook 'after-save-hook 'fvaresi/org-mobile-autopush)

;;;;;;;;;;;;;;;;;
;; Org Notmuch ;;
;;;;;;;;;;;;;;;;;
(require 'org-notmuch)

;;;;;;;;;;;;;;;;;;;;;
;; Org Protocol ;;
;;;;;;;;;;;;;;;;;;;;;

(require 'org-protocol)
(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))

;;;;;;;;;;;;;;
;; Org Todo ;;
;;;;;;;;;;;;;;

(setq org-todo-keywords `((sequence "TODO(t)" "IN_PROGRESS(p)" "|" "DONE(d)")))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

;; for some reason M-S-down and M-S-up are bound to org-shiftmetadown and org-shiftmetaup
(defun fvaresi/org-shiftmetadown-move ()
  (cond
   ((org-at-heading-p) (org-move-subtree-down))
   ((org-at-item-bullet-p) (org-move-item-down))))
(add-hook 'org-shiftmetadown-hook 'fvaresi/org-shiftmetadown-move)

(defun fvaresi/org-shiftmetaup-move ()
  (cond
   ((org-at-heading-p) (org-move-subtree-up))
   ((org-at-item-bullet-p) (org-move-item-up))))
(add-hook 'org-shiftmetaup-hook 'fvaresi/org-shiftmetaup-move)

(defun fvaresi/setup-org ()
  (org-bullets-mode 1))
(add-hook 'org-mode-hook 'fvaresi/setup-org)

(provide 'setup-org)
