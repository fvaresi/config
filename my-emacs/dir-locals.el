;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

(
 ;; (nil
 ;;  ;; projectile test configuration
 ;;  (projectile-project-test-cmd . "vendor/phpunit/phpunit/phpunit --stop-on-failure")

 ;;  ;; indentation settings
 ;;  (tab-width . 4)

 ;;  ;; suppress yasnippet warning
 ;;  (warning-suppress-types . '(yasnippet backquote-change))

 ;;  ;; use vbulletin.el
 ;;  (eval . (load-file "~/projects/config/emacs/vbulletin.el")))

 (js2-mode
  ;; indentation settings
  (indent-tabs-mode . t)
  (js-indent-level . 4))

 ;; (nxml-mode
 ;;  ;; indentation settings
 ;;  (indent-tabs-mode . t)
 ;;  (nxml-child-indent . 4)
 ;;  (sgml-basic-offset . 4))

 (php-mode
  (flycheck-php-phpcs-executable . "/home/user/.config/composer/vendor/bin/phpcs")

  ;; indentation settings
  (indent-tabs-mode . t)
  (c-basic-offset . 4))

 ;; (web-mode
 ;;  ;; indentation settings
 ;;  (indent-tabs-mode . t)
 ;;  (c-basic-offset . 4)
 ;;  (web-mode-markup-indent-offset . 4)
 ;;  (web-mode-code-indent-offset . 4))
 (json-mode
  ;; indentation settings
  (indent-tabs-mode . nil))
 )
