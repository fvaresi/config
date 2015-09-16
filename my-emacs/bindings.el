;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some editing bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<C-return>") 'fvaresi/add-empty-line-after)
(global-set-key (kbd "<C-S-return>") 'fvaresi/add-empty-line-before)

;; joining and splitting lines
(global-set-key (kbd "C-j") 'emmet-expand-line)

(global-set-key (kbd "M-j") 'fvaresi/join-line)

;; (define-key global-map (kbd "M-n") 'forward-sexp)
;; (define-key global-map (kbd "M-p") 'backward-sexp)

(global-set-key (kbd "C-2") 'fvaresi/duplicate-current-line-or-region)

;; transposing functions
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t w") 'transpose-words)

(define-key global-map (kbd "<C-tab>") 'yas-expand-from-trigger-key)

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

(define-key global-map (kbd "C-+") 'text-scale-increase)
;; the following keybindings prevent me to different keyboard layots me
(define-key global-map (kbd "C-*") 'text-scale-decrease)
(define-key global-map (kbd "C-=") 'text-scale-decrease)

;;;;;;;;;;;;;;;;;;;;;;;
;; navigation sanity ;;
;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "M-o") 'mode-line-other-buffer)
;;(global-set-key (kbd "C-S-w") 'toggle-window-split)
(global-set-key (kbd "C-S-r") 'rotate-windows)

(global-set-key (kbd "C-o") 'ace-jump-word-mode)

(define-key isearch-mode-map (kbd "C-q") 'helm-swoop-from-isearch)

(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

(global-set-key (kbd "C-M-g") 'webjump)

;; (global-set-key (kbd "C-x C-0") 'kill-buffer-and-window)

;; (define-key clojure-mode-map (kbd "M-n") 'sp-next-sexp)
;; (define-key clojure-mode-map (kbd "M-p") 'sp-backward-sexp)
;; (define-key clojure-mode-map (kbd "M-u") 'sp-backward-up-sexp)

;; (define-key emacs-lisp-mode-map (kbd "M-n") 'sp-next-sexp)
;; (define-key emacs-lisp-mode-map (kbd "M-p") 'sp-backward-sexp)
;; (define-key emacs-lisp-mode-map (kbd "M-u") 'sp-backward-up-sexp)

;;;;;;;;;;;
;; dired ;;
;;;;;;;;;;;

;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;; (define-key dired-mode-map (kbd "h") 'dired-dotfiles-toggle)

;;;;;;;;;;;;;;;;;;;
;; expand region ;;
;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-S-l") 'helm-locate)
(global-set-key (kbd "C-S-g") 'helm-projectile-grep)
(global-set-key (kbd "C-S-h") 'helm-projectile)

;;;;;;;;;;;
;; hydra ;;
;;;;;;;;;;;

(defhydra hydra-diff-hl (global-map "<f12>")
  "diff-hl"
  ("n" diff-hl-next-hunk)
  ("p" diff-hl-previous-hunk)
  ("v" diff-hl-diff-goto-hunk))

;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors ;;
;;;;;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "C-|") 'mc/mark-next-like-this)

;;;;;;;;;;;;;
;; notmuch ;;
;;;;;;;;;;;;;

(define-key notmuch-search-mode-map (kbd "g") 'notmuch-refresh-this-buffer)
(define-key notmuch-hello-mode-map (kbd "g") 'notmuch-refresh-this-buffer)
(define-key notmuch-search-mode-map "d" 'search-toggle-message-delete)
(define-key notmuch-search-mode-map "D" 'search-toggle-delete-all)
(define-key notmuch-show-mode-map "d" 'show-toggle-message-delete)
(define-key notmuch-show-mode-map "D" 'show-toggle-thread-delete)
(define-key notmuch-tree-mode-map "d" 'tree-toggle-message-delete)
(define-key notmuch-tree-mode-map "D" 'tree-toggle-thread-delete)
(define-key notmuch-search-mode-map "F" 'search-toggle-message-fav)
(define-key notmuch-show-mode-map "F" 'show-toggle-message-fav)
(define-key notmuch-tree-mode-map "F" 'tree-toggle-message-fav)
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

;;;;;;;;;;;;;;
;; org mode ;;
;;;;;;;;;;;;;;

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cd" 'diary)

;;;;;;;;;;;;;
;; paredit ;;
;;;;;;;;;;;;;

(global-set-key (kbd "M-(") 'paredit-wrap-sexp)
(global-set-key (kbd "M-U") 'paredit-splice-sexp-killing-backward)

;;;;;;;;;;;;;;;;;
;; perspective ;;
;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-x x m") 'switch-to-mail-persp)
(define-key global-map (kbd "C-x x t") 'switch-to-twitter-persp)
(define-key global-map (kbd "M-O") 'switch-to-last-persp)

;;;;;;;;;;;;;;;;
;; twittering ;;
;;;;;;;;;;;;;;;;

(define-key twittering-mode-map (kbd "C-S-f") 'twittering-favorite)
(define-key twittering-mode-map (kbd "C-S-r") 'twittering-native-retweet)
