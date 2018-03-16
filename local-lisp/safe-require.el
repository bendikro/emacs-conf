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
		   (message "Error when loading %s" (format "%s" (error-message-string err)))
		   (lambda () )
		   )
		 )))))
