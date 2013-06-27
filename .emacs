;;; Lisp (SLIME) interaction 
(setq inferior-lisp-program "sbcl") 
(add-to-list 'load-path "~/.slime") 
(require 'slime) 
(slime-setup) 

; esto lo agregue yo
(ido-mode t)
(global-linum-mode t)
(column-number-mode t)
(global-visual-line-mode t)
(setq x-select-enable-clipboard t)
(tool-bar-mode nil)

(setq org-log-done 'time)
(server-start)
