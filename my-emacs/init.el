;; Loading custom configuration
(setq custom-file "/home/fvaresi/.emacs.d/custom-configuration.el")
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

			      

			      browse-kill-ring

			      clojure-mode
			      clojure-snippets
			      clojurescript-mode
			      clj-refactor
			      cider
			      ac-cider
			      ac-cider-compliment
			      ac-nrepl
			      ;; cider-browse-ns
			      cider-decompile
			      cider-spy
			      slamhound

			      easy-kill
			      easy-kill-extras

			      emmet-mode

			      expand-region

			      helm
			      helm-swoop
			      helm-projectile

			      json-mode

			      magit

			      multiple-cursors

			      php-mode
			      php-auto-yasnippets

			      popwin

			      perspective
			      projectile
			      persp-projectile

			      rainbow-delimiters

			      smart-mode-line

			      smartparens

			      solarized-theme
			      
			      undo-tree

			      window-number
			      )))
    (dolist (p required-packages)
      (when (not (package-installed-p p))
	(package-install p)))))

(gimme-my-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations of installed packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'solarized-dark t)

(require 'auto-complete-config)
(ac-config-default)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'helm)
(helm-mode t)

(setq helm-swoop-split-direction 'split-window-horizontally)
(setq helm-swoop-speed-or-color nil)

;; fix dead keys
;;(require 'iso-transl)

;; export org files to confluence
;;(require 'ox-confluence)

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

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire nil)
(setq projectile-switch-project-action 'helm-projectile)

(require 'smartparens-config)
(smartparens-global-strict-mode t)
(show-smartparens-global-mode t)
(sp-use-paredit-bindings)

(global-undo-tree-mode t)

(require 'window-number)
(window-number-mode 1)
(window-number-meta-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ac-cider)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)

(eval-after-load "auto-complete"
'(add-to-list 'ac-modes 'cider-mode))

(require 'ac-cider-compliment)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
;;(add-hook 'cider-repl-mode-hook 'ac-cider-compliment-repl-setup)

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)

(require 'cider)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq php-manual-path "/home/fvaresi/php-chunked-xhtml")
(add-hook 'php-mode-hook
 (lambda ()
   (define-key php-mode-map (kbd "C-.") 'er/expand-region)
   (define-key php-mode-map (kbd "C-|") 'mc/mark-next-like-this)
   (define-key php-mode-map (kbd "C-<tab>") 'yas/create-php-snippet)

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

(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "/home/fvaresi/.emacs.d/elpa/php-auto-yasnippets-20140704.1242/Create-PHP-YASnippet.php")



;; Load custom bindings
(load "~/.emacs.d/bindings")
