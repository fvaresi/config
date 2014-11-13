(global-set-key (kbd "M-j") 'jp-join-lines)

;; (define-key global-map (kbd "M-n") 'forward-sexp)
;; (define-key global-map (kbd "M-p") 'backward-sexp)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-S-l") 'helm-locate)
(global-set-key (kbd "C-S-g") 'projectile-ack)
(global-set-key (kbd "C-S-h") 'helm-projectile)

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "C-o") 'ace-jump-word-mode)

(define-key isearch-mode-map (kbd "C-q") 'helm-swoop-from-isearch)

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(global-set-key (kbd "C-|") 'mc/mark-next-like-this)

;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; (define-key dired-mode-map (kbd "h") 'dired-dotfiles-toggle)

;; (global-set-key (kbd "C-x C-0") 'kill-buffer-and-window)
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

;; (define-key clojure-mode-map (kbd "M-n") 'sp-next-sexp)
;; (define-key clojure-mode-map (kbd "M-p") 'sp-backward-sexp)
;; (define-key clojure-mode-map (kbd "M-u") 'sp-backward-up-sexp)

;; (define-key emacs-lisp-mode-map (kbd "M-n") 'sp-next-sexp)
;; (define-key emacs-lisp-mode-map (kbd "M-p") 'sp-backward-sexp)
;; (define-key emacs-lisp-mode-map (kbd "M-u") 'sp-backward-up-sexp)
