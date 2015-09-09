;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
(setq inhibit-startup-screen t)

;; Set path to dependencies
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)

;; Loading custom configuration
(setq custom-file (expand-file-name "custom-configuration.el" user-emacs-directory))
(load custom-file)

;; Package managing
(require 'setup-package)

;; Set up appearance early
(require 'appearance)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations of installed packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; apt notifications
(require 'notifications)
(defun my-appt-window-function (min-to-app new-time msg)
  (if (atom min-to-app)
      (notifications-notify :title (format "In %s minutes" min-to-app)
			    :body msg)
    (dolist (i (number-sequence 0 (1- (length min-to-app))))
      (notifications-notify :title (format "In %s minutes" (nth i min-to-app))
			    :body (nth i msg))
      )))

(appt-activate t)
(setq appt-disp-window-function 'my-appt-window-function)
(setq appt-delete-window-function nil)

;; backup & autosave customizations
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory temporary-file-directory)
(setq tramp-use-ssh-controlmaster-options nil)

(setq browse-url-generic-program (executable-find "conkeror"))
(setq browse-url-browser-function 'browse-url-generic)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(delete-selection-mode t)
(setq desktop-save-mode t)

(setq diary-comment-start "##")

(require 'helm)
(helm-mode t)

(setq helm-mode-fuzzy-match t)
(setq helm-swoop-split-direction 'split-window-horizontally)
(setq helm-swoop-speed-or-color nil)

(require 'hydra)

(setq initial-scratch-message nil)

(setq-default ispell-program-name "aspell")

;; fix dead keys
;;(require 'iso-transl)

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x v"))
(guide-key-mode 1)

(setq magit-use-overlays nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-push-always-verify nil)

(eval-after-load 'org '(require 'setup-org))

(setq paradox-automatically-star t)

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
(setq projectile-svn-command "find . -type f -print0")
(setq projectile-switch-project-action 'projectile-dired)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)		

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

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

(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Android
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'android-mode)
(setq android-mode-sdk-dir "~/opt/android-sdk-linux")
(setq android-mode-builder 'gradle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-company-mode)

(require 'cider)

(setq cider-repl-history-file "~/.emacs.d/cider-repl-history")
(setq nrepl-sync-request-timeout 40) ;; Since boot takes some time to load, cider must wait for it...

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (eldoc-mode 1)

  (setq cider-repl-display-in-current-window t)
  
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m")

  (turn-on-diff-hl-mode))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Email
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'setup-email)

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

(defun fvaresi/setup-php ()
   (yas-minor-mode 1)

   (turn-on-diff-hl-mode)

   ;; (whitespace-mode t)
   ;; (setq whitespace-style '(face tabs spaces trailing identation))

   (flycheck-mode t)
   
   (define-key php-mode-map (kbd "C-.") 'er/expand-region)
   (define-key php-mode-map (kbd "C-|") 'mc/mark-next-like-this)
   (define-key php-mode-map (kbd "C-<tab>") 'yas/create-php-snippet)
   (define-key php-mode-map (kbd "M-j") 'fvaresi/join-line)

   (c-set-style "bsd")
   (setq c-basic-offset 4)
   (setq indent-tabs-mode t)
   (setq tab-width 4)

   (setq comment-multi-line nil ;; maybe
        comment-start "// "
        comment-end ""
        comment-style 'indent
        comment-use-syntax t))
(add-hook 'php-mode-hook 'fvaresi/setup-php)

(setq geben-display-window-function 'switch-to-buffer)
(setq geben-pause-at-entry-line nil)
(setq geben-show-breakpoints-debugging-only nil)
(setq geben-source-coding-system 'iso-8859-1)

(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "~/.emacs.d/elpa/php-auto-yasnippets-20141128.1411/Create-PHP-YASnippet.php")

;; (require 'php-refactor-mode)
;; (add-hook 'php-mode-hook 'php-refactor-mode)
;; (setq php-refactor-command "refactor.phar")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQLi
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq sql-mysql-login-params
      '((user :default "forum")
	(password)
	(database)
	(server :default "localhost")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Twitter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'twittering-mode)
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)
(setq twittering-edit-skeleton 'inherit-any)

(defun switch-to-twitter-persp ()
  (interactive)
  (persp-switch "twitter")
  (twit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load custom bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "~/.emacs.d/bindings")
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
