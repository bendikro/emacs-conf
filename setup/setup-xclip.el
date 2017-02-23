(require 'xclip)

(setq x-select-enable-clipboard t)

(defun copy-to-clipboard ()
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

(defun paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
	  (progn
		(clipboard-yank)
		(message "graphics active")
		)
	  (progn
		(message "Call xclip-selection-value")
		;;(xclip-selection-value)
		(xclip-selection-value)
		(message "Called xclip-selection-value")
		)
	)
  )

(provide 'setup-xclip)
