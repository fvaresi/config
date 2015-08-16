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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations of installed packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq appt-activate t)

;; backup & autosave customizations
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq tramp-backup-directory-alist backup-directory-alist)
(setq tramp-auto-save-directory temporary-file-directory)

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
(setq projectile-file-exists-remote-cache-expire nil)
(setq projectile-svn-command "find . -type f -print0")
;;(setq projectile-switch-project-action 'helm-projectile)
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
;;; Magic editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
	   (end (or end (region-end)))
	   (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
	(insert region)))))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (duplicate-region num (point-at-bol) (1+ (point-at-eol)))))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(duplicate-region arg beg end)
	(one-shot-keybinding "d" (λ (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window juggling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

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
  
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

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
(add-hook 'php-mode-hook
 (lambda ()
   (yas-minor-mode 1)

   (turn-on-diff-hl-mode)

   ;; (whitespace-mode t)
   ;; (setq whitespace-style '(face tabs spaces trailing identation))

   (flycheck-mode t)
   
   (define-key php-mode-map (kbd "C-.") 'er/expand-region)
   (define-key php-mode-map (kbd "C-|") 'mc/mark-next-like-this)
   (define-key php-mode-map (kbd "C-<tab>") 'yas/create-php-snippet)
   (define-key php-mode-map (kbd "M-j") (lambda ()
					  (interactive)
					  (join-line -1)))

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

(server-start)
