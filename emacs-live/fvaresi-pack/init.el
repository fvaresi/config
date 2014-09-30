;; User pack init file
;;
;; Use this file to initiate the pack configuration.
;; See README for more information.

;; Load bindings config
(live-load-config-file "bindings.el")

(setq custom-file "/home/fvaresi/.live-packs/fvaresi-pack/custom-configuration.el")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(live-set-default-font "DejaVu Sans Mono-9")

;; popwin settings
(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
(push '("*Helm Find Files*" :height 0.5) popwin:special-display-config)
(push '("*helm mini*" :height 0.5) popwin:special-display-config)
(push '("*helm grep*" :height 0.5) popwin:special-display-config)
(push '("*helm locate*" :height 0.5) popwin:special-display-config)
(push '("*helm M-x*" :height 0.5) popwin:special-display-config)
(push '("*helm projectile*" :height 0.5) popwin:special-display-config)
(push '("*helm etags*" :height 0.5) popwin:special-display-config)

;; php-mode settings
(add-hook 'php-mode-hook
 (lambda ()
   (define-key php-mode-map (kbd "C-.") 'er/expand-region)
   (define-key php-mode-map (kbd "C-t") 'mc/mark-next-like-this)
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

;; export org files to confluence
(require 'ox-confluence)

;; TODO: still need to find a way of packing the default plugings:
;; php-mode, helm, minimap, popwin, projectile, helm-projectile, multi-term


;; Helm for IB shells
(setq vb-hosts-candidates '("chevyhhr-web1"
                            "stg-audiworld1"))


(setq helm-source-vb-hosts
  '((name . "Autocomm Sites")
    (candidates . vb-hosts-candidates)
    (pattern-transformer . (lambda (pattern) (regexp-quote pattern)))
    (action . identity)))

(defun go-to-vb-host (selected-host)
  (interactive (list (helm :sources 'helm-source-vb-hosts)))
  (find-file (format "/ssh:%s:/" selected-host)))

;; solarized theme
(load-theme 'solarized-dark t)

;; fix dead keys
(require 'iso-transl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; projectile settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire nil)
(setq projectile-switch-project-action 'helm-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perspective
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'perspective)
(persp-mode)
(require 'persp-projectile)
(projectile-persp-bridge helm-projectile)


;; (defvar helm-source-projectile-dot-patch
;;   `((name . "Projectile Dot patch")
;;     (candidates . ("./"))
;;     (keymap . ,helm-generic-files-map)
;;     (help-message . helm-find-file-help-message)
;;     (mode-line . helm-ff-mode-line-string)
;;     (type . file)
;;     (action . (("Find file" . (lambda (file) (find-file file)))
;;                ("Open dired in file's directory" . helm-open-dired))))
;;   "Helm source definition.")
;; (setq helm-projectile-sources-list (quote (helm-source-projectile-dot-patch helm-source-projectile-files-list helm-source-projectile-buffers-list helm-source-projectile-recentf-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multi-term settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'multi-term)

(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

(add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab))
(add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
(add-to-list 'term-bind-key-alist '("C-c C-k" . term-char))

;; fix for bug when hitting enter in the middle of the line
(defadvice term-send-input (before dirty-hack activate)
  (end-of-line)
  (set-marker (process-mark (get-buffer-process (current-buffer)))
			  (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; php-auto-yasnippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "/home/fvaresi/.emacs.d/elpa/php-auto-yasnippets-20140704.1242/Create-PHP-YASnippet.php")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; easy-kill
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-swoop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq helm-swoop-split-direction 'split-window-horizontally)
(setq helm-swoop-speed-or-color nil)


(global-set-key (kbd "C-6") 'yas-expand)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; twittering-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'twittering-mode)
(setq twittering-use-master-password t)
