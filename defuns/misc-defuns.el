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
