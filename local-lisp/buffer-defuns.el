;; Buffer-related defuns

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8"
  (interactive)
  (setq tab-width (if (= tab-width 8) 4 8))
  (setq c-basic-offset tab-width)
  (redraw-display))

(defun toggle-indent-tabs-mode ()
  "Toggle setting indent-tabs-mode"
  (interactive)
  (setq indent-tabs-mode (if (eq indent-tabs-mode t) nil t))
  (message "New indent-tabs-mode: %s" indent-tabs-mode))

(defun follow-buffer (&optional buffer)
    "Sets the cursor to the end to make sure the buffer is followed"
    (interactive)
	(if (eq buffer nil)
		(setq buffer (current-buffer)))
	(let ((win (get-buffer-window buffer)))
	    (if (eq win nil)
			(with-current-buffer buffer
			  (goto-char (point-max)))
		  (with-selected-window
			  (get-buffer-window buffer)
			(goto-char (point-max))))))

(defun follow-auctex-compile-buffer ()
    "Sets the cursor to the end to make sure the buffer is followed"
    (interactive)
	;; This should give the compilation buffer
	(message "(TeX-active-buffer): %s" (TeX-active-buffer))
	(let ((buffer (TeX-active-buffer)))
	  (if buffer
		  (if (with-current-buffer buffer
				(follow-buffer))
			  (message "Following buffer: %s" buffer)
			(message "No process for this document.")))))

(defun backward-whitespace (arg)
  "Move point to the beginning of the current sequence of whitespace characters."
  (interactive "^p")
  (forward-whitespace (- arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll up/down and keep cursor position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scroll the text one line down while keeping the cursor
(defun scroll-down-keep-cursor ()
   (interactive)
   (scroll-down 1))

;; Scroll the text one line up while keeping the cursor
(defun scroll-up-keep-cursor ()
   (interactive)
   (scroll-up 1))

(require 'whitespace)

(setq whitespace-display-mappings
 '(
   (space-mark 32 [183] [46]) ; normal space
   (space-mark 160 [164] [95])
   (space-mark 2208 [2212] [95])
   (space-mark 2336 [2340] [95])
   (space-mark 3616 [3620] [95])
   (space-mark 3872 [3876] [95])
   (newline-mark 10 [182 10]) ; line feed
   (newline-mark 13 [181 13]) ; carriage return
   (tab-mark 9 [9655 9] [92 9]) ; tab
))


;; Comment or incomment region
(defun comment-or-uncomment-line-or-region ()
  (interactive)
  (if (use-region-p)
	  (comment-or-uncomment-region (region-beginning) (region-end))
	(comment-or-uncomment-region (line-beginning-position) (line-end-position)
								 )))
