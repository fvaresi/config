(global-set-key (kbd "C-j") 'emmet-expand-line)

(global-set-key (kbd "<C-return>")
		(lambda ()
		  (interactive)
		  (end-of-line)
		  (newline)
		  (indent-for-tab-command)))

(global-set-key (kbd "<C-S-return>")
		(lambda ()
		  (interactive)
		  (beginning-of-line)
		  (newline)
		  (forward-line -1)
		  (indent-for-tab-command)))

(global-set-key (kbd "M-j")
		(lambda ()
		  (interactive)
		  (join-line -1)))

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

(define-key global-map "\C-cc" 'org-capture)

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

(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

(global-set-key (kbd "C-M-g") 'webjump)


(define-key global-map (kbd "C-x x m") 'switch-to-mail-persp)
(define-key notmuch-search-mode-map (kbd "g") 'notmuch-refresh-this-buffer)
(define-key notmuch-hello-mode-map (kbd "g") 'notmuch-refresh-this-buffer)
(define-key notmuch-search-mode-map "d" 'search-toggle-message-delete)
(define-key notmuch-show-mode-map "d" 'show-toggle-message-delete)
(define-key notmuch-tree-mode-map "d" 'tree-toggle-message-delete)
(define-key notmuch-tree-mode-map "D" 'tree-toggle-thread-delete)
(define-key notmuch-search-mode-map "u" 'search-toggle-message-unread)
(define-key notmuch-show-mode-map "u" 'show-toggle-message-unread)
(define-key notmuch-search-mode-map "y" 'search-toggle-message-inbox)
(define-key notmuch-show-mode-map "y" 'show-toggle-message-inbox)
(define-key notmuch-tree-mode-map "y" 'tree-toggle-thread-inbox)
(define-key notmuch-show-mode-map "h" 'show-email-externally)
(define-key notmuch-show-mode-map "H" 'show-email-externally-full-thread)
(define-key notmuch-search-mode-map "r" 'reply-to-thread-sender-search)
(define-key notmuch-show-mode-map "r" 'reply-to-thread-sender-show)
(define-key notmuch-search-mode-map "R" 'reply-to-thread-search)
(define-key notmuch-show-mode-map "R" 'reply-to-thread-show)
(define-key notmuch-search-mode-map "T" 'notmuch-jump-to-tag)

(define-key global-map (kbd "C-x x t") 'switch-to-twitter-persp)
(define-key twittering-mode-map (kbd "C-S-f") 'twittering-favorite)
(define-key twittering-mode-map (kbd "C-S-r") 'twittering-native-retweet)

(define-key global-map (kbd "<C-tab>") 'yas-expand-from-trigger-key)
