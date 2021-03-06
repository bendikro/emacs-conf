;; Misc defuns go here
;; It wouldn't hurt to look for patterns and extract once in a while


(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    Custom funcs   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar do-show-messages t
  "Current buffer.")

(defun reload-config ()
  "reload your .emacs file without restarting Emacs"
  (interactive)
  (setq do-show-messages nil)
  (load-file user-init-file)
  (setq do-show-messages t))

(defun startup-with-messages-buffer ()
  (interactive)
  ;; If Messages buffer is not shown in a window, split window, and show it
  (unless (get-buffer-window "*Messages*")
	 (split-window-horizontally)
	 (next-multiframe-window)
	 (switch-to-buffer (get-buffer-create "*Messages*"))
	 (next-multiframe-window)
	 ))

(defun messages ()
  (interactive)
  (message "Messages do-show-messages: %s" do-show-messages)
  (if do-show-messages
	  (progn
		(message "Showing messages buffer!")
		(split-window-horizontally)   ;; want two windows at startup
		(other-window 1)              ;; move to other window
		(let ((buf (get-buffer "*Messages*")))
		  (if buf
			  (switch-to-buffer buf)))
		(other-window 1)              ;; move to other window
		)))


(defun ignore-error-wrapper (fn)
  "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
  (lexical-let ((fn fn))
	(lambda ()
	  (interactive)
	  (ignore-errors
		(funcall fn)))))

(defmacro safe-wrap (fn &rest clean-up)
  "Funtion that runs code and catches errors. In case of error, run
  the clean-up code.
  Example: (safe-wrap (error \"Hello\") (message \"Error occured...\"))
"
  `(unwind-protect
	   (let (retval)
		 (condition-case ex
			 (setq retval (progn ,fn))
		   ('error
			(message (format "Caught exception: [%s]" ex))
			(setq retval (cons 'exception (list ex)))
			,@clean-up))
		 retval)))

;; Add function for multiple hooks
(defun add-hooks (function hooks)
  (mapc (lambda (hook)
		  (add-hook hook function))
		hooks))

(defmacro measure-time (comment &rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
	 ,@body
	 (message "%s took %.06f seconds" ,comment (float-time (time-since time)))
	 ))


(defun fill-sentence()
  "Works like fill-paragraph but starts from the current sentence"
  (interactive)
  (save-excursion
    (or (eq (point) (point-max)) (forward-char))
	(back-to-indentation)
    (indent-relative t)
	(let ((beg (point)))
	  (forward-paragraph)
	  (fill-region-as-paragraph beg (point)))))
