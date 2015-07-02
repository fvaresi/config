(require 'package)

;; Add marmalade and melpa to package repos
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

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
			      
			      hydra
			      
			      helm
			      helm-swoop
			      helm-projectile

			      json-mode

			      magit

			      multiple-cursors

			      notmuch

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

			      smartparens

			      smooth-scrolling

			      solarized-theme
			      
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
