(defgroup safeloadfiles nil
  "Group for files in safeload files group."
  :group 'convenience)

(defun require-safe (filename)
  "As `load', but if a variable safe-load-<fielname> is nil, only print a message."
(setq safe-load-varname  (concat "safe-load-"  filename))
(setq safe-load-varname-value (car (get (intern safe-load-varname) 'standard-value)))
  (condition-case err
	  (load filename)
	(error
	 (progn
	   (if safe-load-varname-value
		   (message "Error loading file \"%s\": %s" filename
					(error-message-string err))
		 (progn
		   (signal
			(error "%s\n%s" (error-message-string err)
				   (format "To hide this message, add the following to '%s':\n%s"
						   "init-local.el"
						   (format "(defcustom %s t
  \"Show warning if file fails to load\"
  :group 'safeloadfiles
  :type 'boolean))"
								   safe-load-varname)))
			(cdr err)
			))))
	 nil)))
