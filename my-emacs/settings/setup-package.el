(require 'package)

;; Add marmalade and melpa to package repos
;; (add-to-list 'package-archives
;;              '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(defun gimme-my-packages ()
  (interactive)
  (let (( required-packages '(
			      ace-isearch
			      ace-jump-mode

			      ack-and-a-half

			      android-mode

			      browse-kill-ring
			      
			      conkeror-minor-mode

			      clojure-mode
			      clojure-snippets
			      clojurescript-mode
			      clj-refactor
			      cider
			      cider-decompile
			      cider-spy
			      company
			      slamhound
			      4clojure

			      diff-hl
			      
			      easy-kill
			      easy-kill-extras

			      emmet-mode

			      expand-region

			      flycheck

			      geben

			      gh-md

			      gnus-alias
			      
			      guide-key
			      
			      hydra
			      
			      helm
			      helm-swoop
			      helm-projectile

			      json-mode

			      magit

			      multiple-cursors

			      notmuch

			      org-plus-contrib
			      ob-http
			      org-jira
			      org-beautify-theme

			      paradox

			      paredit

			      php-mode
			      php-auto-yasnippets
			      php-refactor-mode

			      popwin

			      perspective
			      projectile
			      persp-projectile

			      rainbow-delimiters

			      restclient

			      smart-mode-line
			      smart-mode-line-powerline-theme

			      smartparens

			      smooth-scrolling

			      solarized-theme

			      string-edit
			      
			      twittering-mode
			      
			      undo-tree

			      web-beautify

			      window-number
			      )))
    (dolist (p required-packages)
      (when (not (package-installed-p p))
	(package-install p)))))

;;(gimme-my-packages)

(provide 'setup-package)
