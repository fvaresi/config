;; This was required to avoid an error with org mode.
(setq package-enable-at-startup nil)
(package-initialize)

(org-babel-load-file (expand-file-name "main.org" user-emacs-directory))
