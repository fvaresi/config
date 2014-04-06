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

(defun my-grep () (interactive)
       (helm-do-grep-1 '("/media/datos/Trabajo/autocomm/vBulletinPlugins") t nil '("*.php" "*.xml")))

(live-set-default-font "DejaVu Sans Mono-9")

(push '(" *undo-tree*" :width 0.3 :position right) popwin:special-display-config)
(push '("*Helm Find Files*" :height 0.5) popwin:special-display-config)
(push '("*helm mini*" :height 0.5) popwin:special-display-config)
(push '("*helm grep*" :height 0.5) popwin:special-display-config)
(push '("*helm locate*" :height 0.5) popwin:special-display-config)
(push '("*helm M-x*" :height 0.5) popwin:special-display-config)

(add-hook 'php-mode-hook
 (lambda ()
   (define-key php-mode-map (kbd "C-.") 'er/expand-region)
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

(require 'ox-confluence)

;; TODO: still need to find a way of packing the default plugings:
;; php-mode, helm, minimap, popwin


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
