;;;;;;;;;;;;;;;;;;;;
;; Editing defuns ;;
;;;;;;;;;;;;;;;;;;;;

(defun fvaresi/add-empty-line-after ()
  "Add empty line after current one"
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun fvaresi/add-empty-line-before ()
  "Add empty line before current one"
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun fvaresi/join-line ()
  "Join lines"
  (interactive)
  (join-line -1))

(defun fvaresi/duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
	   (end (or end (region-end)))
	   (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
	(insert region)))))

(defun fvaresi/duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (fvaresi/duplicate-region num (point-at-bol) (1+ (point-at-eol)))))

(defun fvaresi/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
	    (end (region-end)))
	(duplicate-region arg beg end)
	(one-shot-keybinding "d" (λ (fvaresi/duplicate-region 1 beg end))))
    (fvaresi/duplicate-current-line arg)
    (one-shot-keybinding "d" 'fvaresi/duplicate-current-line)))
