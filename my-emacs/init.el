;; This was required to avoid an error with org mode.
(setq package-enable-at-startup nil)
(package-initialize)

(setq vc-follow-symlinks t)

(org-babel-load-file (expand-file-name "main.org" user-emacs-directory))
