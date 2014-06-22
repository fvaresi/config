;; Place your bindings here.

;; For example:
;;(define-key global-map (kbd "C-+") 'text-scale-increase)
;;(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x t") 'multi-term)

(global-set-key (kbd "C-i") 'indent-region)

(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)
(global-set-key (kbd "M-,") 'pop-tag-mark)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-S-l") 'helm-locate)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-S-h") 'helm-projectile)
(global-set-key (kbd "C-S-g") 'projectile-grep)

(global-set-key (kbd "<f2>") 'minimap-create)
(global-set-key (kbd "<f3>") 'minimap-kill)

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
