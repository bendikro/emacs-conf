;; Defuns for working with files

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new (if eproject-mode
                  (s-chop-prefix (eproject-root) filename)
                filename))))

(defun find-or-create-file-at-point ()
  "Guesses what parts of the buffer under point is a file name and opens it."
  (interactive)
  (find-file (file-name-at-point)))

(defun find-or-create-file-at-point-other-window ()
  "Guesses what parts of the buffer under point is a file name and opens it."
  (interactive)
  (find-file-other-window (file-name-at-point)))

(defun file-name-at-point ()
  (save-excursion
    (let* ((file-name-regexp "[./a-zA-Z0-9\-_~]")
           (start (progn
                    (while (looking-back file-name-regexp)
                      (forward-char -1))
                    (point)))
           (end (progn
                  (while (looking-at file-name-regexp)
                    (forward-char 1))
                  (point))))
      (buffer-substring start end))))

(defun touch-buffer-file ()
  (interactive)
  (insert " ")
  (backward-delete-char 1)
  (save-buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;    Set buffer file writable (chmod)     ;;;;;;
;;;;;    with M-x RET set-writable            ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(defun unix-output (command &rest args)
  "Run a unix command and, if it returns 0, return the output as a string.
Otherwise, signal an error.  The error message is the first line of the output."
  (let ((output-buffer (generate-new-buffer "*stdout*")))
    (unwind-protect
     (let ((return-value (apply 'call-process command nil
                                    output-buffer nil args)))
       (save-excursion
         (set-buffer output-buffer)
         (unless (= return-value 0)
           (goto-char (point-min))
           (end-of-line)
           (if (= (point-min) (point))
                  (error "Command failed: %s%s" command
                           (with-output-to-string
                                   (dolist (arg args)
                                     (princ " ")
                                     (princ arg))))
                (error "%s" (buffer-substring-no-properties (point-min)
                                                                   (point)))))
         (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer output-buffer))))

(defun trim-right (bag string &optional start end)
  (setq bag (if (eq bag t) '(?\  ?\n ?\t ?\v ?\r ?\f) bag)
    start (or start 0)
    end (or end (length string)))
  (while (and (> end 0)
          (member (aref string (1- end)) bag))
    (decf end))
  (subseq string start end))

(defun set-writable()
  "Make the file shown in the current buffer writable.
Make the buffer writable as well."
  (interactive)
  (unix-output "chmod" "+w" (buffer-file-name))
  (toggle-read-only nil)
  (message (trim-right '(?\n) (unix-output "ls" "-l" (buffer-file-name)))))

;(defun sudo-before-save-hook ()
;  (set (make-local-variable 'sudo:file) (buffer-file-name))
;  (when sudo:file
;    (unless(file-writable-p sudo:file)
;      (set (make-local-variable 'sudo:old-owner-uid) (nth 2 (file-attributes sudo:file)))
;      (when (numberp sudo:old-owner-uid)
;		(unless (= (user-uid) sudo:old-owner-uid)
;		  (when (y-or-n-p
;				 (format "File %s is owned by %s, save it with sudo? "
;						 (file-name-nondirectory sudo:file)
;						 (user-login-name sudo:old-owner-uid)))
;			(sudo-chown-file (int-to-string (user-uid)) (sudo-quoting sudo:file))
;			(add-hook 'after-save-hook
;					  (lambda ()
;						(sudo-chown-file (int-to-string sudo:old-owner-uid)
;										 (sudo-quoting sudo:file))
;						(if sudo-clear-password-always
;							(sudo-kill-password-timeout)))
;					  nil   ;; not append
;					  t    ;; buffer local hook
;					  )))))))
;
;(require 'sudo)
;(add-hook 'before-save-hook 'sudo-before-save-hook)

(defun create-if-no-exists (dirs)
  (loop for d in dirs do
		(if (not (file-exists-p d))
			(progn
			  ;(message "Not exists: %s" d)
			  (make-directory d t)))))

(provide 'file-defuns)
