(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
;;(set-face-attribute 'default nil :font "Source Code Pro-10")

(setq frame-title-format "%f")

(load-theme 'solarized-light t)

(require 'smart-mode-line)
(sml/setup)
(sml/apply-theme 'respectful)	      

(add-to-list 'rm-excluded-modes " yas")
(add-to-list 'rm-excluded-modes " company")
(add-to-list 'rm-excluded-modes " Undo-Tree")
(add-to-list 'rm-excluded-modes " FlyC")
(add-to-list 'rm-excluded-modes " Guide")
(add-to-list 'rm-excluded-modes " Helm")
(add-to-list 'rm-excluded-modes " SP/s")

(provide 'appearance)

