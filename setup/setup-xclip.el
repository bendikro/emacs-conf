(require 'xclip nil 'noerror)

(setq x-select-enable-clipboard t)

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
