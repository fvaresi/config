;# -*- mode: lisp-interaction -*-

;; Lisp (SLIME) interaction 
(setq inferior-lisp-program "sbcl") 
(add-to-list 'load-path "~/.slime") 
(require 'slime) 
(slime-setup) 

; esto lo agregue yo
(ido-mode t)
(global-linum-mode t)
;(column-number-mode t)
;(global-visual-line-mode t)
(setq x-select-enable-clipboard t)
(tool-bar-mode nil)

(setq org-log-done 'time)
(server-start)

(setq line-move-visual nil)

(add-to-list 'load-path "~/.emacs.d/")
;; Any add to list for package-archives (to add marmalade or melpa) goes here
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("06f5145c01ec774a0abb49eeffa3980743ce2f997112b537effeb188b7c51caf" default)))
 '(inhibit-startup-screen t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))

(require 'php-mode)
(setq php-mode-force-pear t)
(add-hook 'php-mode-hook
      '(lambda ()
         (setq indent-tabs-mode t)
         (setq tab-width 4)
         (setq c-basic-offset 4)))
