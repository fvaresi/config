;; This was required to avoid an error with org mode.
(setq package-enable-at-startup nil)
(package-initialize)

(setq vc-follow-symlinks t)

;; Before tangling files, decrypt as required.
(require 'org-crypt)

(org-crypt-use-before-save-magic)

;; Prevent already encrypted text being encrypted again.
(setq org-tags-exclude-from-inheritance '("crypt"))

(setq org-crypt-key "0693BED7")

;; Auto-saving does not cooperate with org-crypt.el: so you need
;; to turn it off if you plan to use org-crypt.el quite often.
;; Otherwise, you'll get an (annoying) message each time you
;; start Org.
(setq auto-save-default nil)

(add-hook 'org-babel-pre-tangle-hook 'org-decrypt-entries t)

(org-babel-load-file (expand-file-name "main.org" user-emacs-directory))
