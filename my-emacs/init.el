;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-screen t)

;; Loading custom configuration
(setq custom-file "~/.emacs.d/custom-configuration.el")
(load custom-file)

(set-face-attribute 'default nil :font "DejaVu Sans Mono-9")

;; Package managing
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(defun gimme-my-packages ()
  (interactive)
  (let (( required-packages '(
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

			      easy-kill
			      easy-kill-extras

			      emmet-mode

			      expand-region

			      flycheck

			      geben

			      helm
			      helm-swoop
			      helm-projectile

			      json-mode

			      magit

			      multiple-cursors

			      php-mode
			      php-auto-yasnippets
			      php-refactor-mode

			      popwin

			      perspective
			      projectile
			      persp-projectile

			      rainbow-delimiters

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations of installed packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'solarized-dark t)

(require 'auto-complete-config)
(ac-config-default)

;; backup customizations
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq tramp-backup-directory-alist backup-directory-alist)

(setq browse-url-generic-program (executable-find "conkeror"))
(setq browse-url-browser-function 'browse-url-generic)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(delete-selection-mode t)
(setq desktop-save-mode t)

(require 'helm)
(helm-mode t)

(setq helm-swoop-split-direction 'split-window-horizontally)
(setq helm-swoop-speed-or-color nil)

;; fix dead keys
;;(require 'iso-transl)

;; export org files to confluence
;;(require 'ox-confluence)

(setq magit-use-overlays nil)

;; org capture
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(require 'perspective)
(persp-mode)
(require 'persp-projectile)
(projectile-persp-bridge helm-projectile)

(require 'popwin)
(popwin-mode t)
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
(push '("*Helm Find Files*" :height 0.5) popwin:special-display-config)
(push '("*helm mini*" :height 0.5) popwin:special-display-config)
(push '("*helm grep*" :height 0.5) popwin:special-display-config)
(push '("*helm locate*" :height 0.5) popwin:special-display-config)
(push '("*helm M-x*" :height 0.5) popwin:special-display-config)
(push '("*helm projectile*" :height 0.5) popwin:special-display-config)
(push '("*helm etags*" :height 0.5) popwin:special-display-config)
(push '("*Ack-and-a-half*" :height 0.5 :stick t) popwin:special-display-config)

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire nil)
(setq projectile-switch-project-action 'helm-projectile)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)		

(setq initial-scratch-message nil)

(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'respectful)	      

(require 'smartparens-config)
(smartparens-global-strict-mode t)
(show-smartparens-global-mode t)
(sp-use-paredit-bindings)

(require 'smooth-scrolling)

(global-undo-tree-mode t)

(require 'window-number)
(window-number-mode 1)
(window-number-meta-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Android
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'android-mode)
(custom-set-variables '(android-mode-sdk-dir "~/opt/android-sdk-linux"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-company-mode)

(require 'cider)

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-repl-history-file "~/.emacs.d/cider-repl-history")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(setq emmet-move-cursor-between-quotes t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq php-manual-path "~/php-chunked-xhtml")
(add-hook 'php-mode-hook
 (lambda ()
   (yas-minor-mode 1)
   
   (define-key php-mode-map (kbd "C-.") 'er/expand-region)
   (define-key php-mode-map (kbd "C-|") 'mc/mark-next-like-this)
   (define-key php-mode-map (kbd "C-<tab>") 'yas/create-php-snippet)
   (define-key php-mode-map (kbd "M-j") 'jp-join-lines)

   (c-set-style "bsd")
   (setq c-basic-offset 4)
   (setq indent-tabs-mode t)
   (setq tab-width 4)

   (setq comment-multi-line nil ;; maybe
        comment-start "// "
        comment-end ""
        comment-style 'indent
        comment-use-syntax t)
 )
)

(setq geben-pause-at-entry-line nil)

(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "~/.emacs.d/elpa/php-auto-yasnippets-20141128.1411/Create-PHP-YASnippet.php")

;; (require 'php-refactor-mode)
;; (add-hook 'php-mode-hook 'php-refactor-mode)
;; (setq php-refactor-command "refactor.phar")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Twitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)
(setq twittering-edit-skeleton 'inherit-any)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/bindings")
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
