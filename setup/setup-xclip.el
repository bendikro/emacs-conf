(if (require 'xclip nil 'noerror)
	(message "xclip found")
  (message "Failed to load xclip"))

(setq x-select-enable-clipboard t)

(defun xclip-select-text (text)
  "Copied from xclip.el Version: 1.10 to avoid 'void-function xclip-select-text'"
  (xclip-set-selection 'primary text)
  (setq xclip-last-selected-text-primary text)
  (when xclip-select-enable-clipboard
	(xclip-set-selection 'clipboard text)
      (setq xclip-last-selected-text-clipboard text)))

(defun clipb-copy ()
  "Copies selection to x-clipboard."
  (interactive)
  (if (display-graphic-p)
	(progn
	  (call-interactively 'clipboard-kill-ring-save)
	  (message "Copied region to x-clipboard")
	  )
	(if (region-active-p)
		(let (
			  (selection (buffer-substring-no-properties (region-beginning) (region-end))))
		  (if (= (length selection) 0)
			  (message "No region active; can't copy to clipboard")
			(progn
			  (xclip-select-text selection)
			  (message "Copied region to clipboard")
			  (deactivate-mark)
			  )
			)
		  )
	  )
	)
  )

(defun clipb-paste ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
	  (clipboard-yank)
	(progn
	  (xclip-selection-value)
	  (insert xclip-last-selected-text-clipboard)
	  )
	)
  )

(provide 'setup-xclip)
