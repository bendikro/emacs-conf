;; Buffer-related defuns

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))


(defun tf-toggle-tab-width-setting ()
    "Toggle setting tab widths between 4 and 8"
    (interactive)
    (setq tab-width (if (= tab-width 8) 4 8))
    (setq c-basic-offset tab-width)
    (redraw-display))
