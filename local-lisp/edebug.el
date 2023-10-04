;; we are using eldoc-current-symbol from eldoc
;;(require 'eldoc)

(defun edebug-elisp-add-to-watch (&optional region-start region-end)
  "Add the current variable to the *EDebug* window"
  (interactive "r")
  (let ((statement
		 (if (and region-start region-end (use-region-p))
			 (buffer-substring region-start region-end)
		   (symbol-name (elisp--current-symbol)))))
		   ;;(symbol-name (eldoc-current-symbol)))))

	;; open eval buffer
	(edebug-visit-eval-list)
	;; jump to the end of it and add a newline
	(goto-char (point-max))
	(newline)
	;; insert the variable
	(insert statement)
	;; update the list
	(edebug-update-eval-list)
	;; jump back to where we were
	(edebug-where)))

;; define a key
(add-hook 'edebug-setup-hook
		  #'(lambda() (define-key edebug-mode-map "A" 'edebug-elisp-add-to-watch)))

(defmacro stop-here (fn)
  "Call edebug here. FN is assumed to be a symbol of the function you are in."
  `(if (consp (get ,fn 'edebug))
	   (edebug)))

(defun clear-edebug (fn-sym)
  "Remove 'edebug property from FN-SYM, a function symbol."
  (put fn-sym 'edebug nil))



(defun bro-indent-for-tab-command (&optional arg)
  (message "bro-indent-for-tab-command")
  (interactive "P")
  (indent-for-tab-command arg))
;;  (put fn-sym 'edebug nil))


;;(global-set-key (kbd "TAB") 'indent-for-tab-command)
