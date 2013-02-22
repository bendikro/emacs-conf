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

(defun delete-word-no-copy (arg)
  "Delete characters forward until encountering the end of a word.
   With argument, do this that many times.
   This command does not push erased text to kill-ring."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word-no-copy (arg)
  "Delete characters backward until encountering the beginning of a word.
   With argument, do this that many times.
   This command does not push erased text to kill-ring."
  (interactive "p")
  (delete-word-no-copy (- arg)))

(defun copy-rectangle-to-clipboard (p1 p2)
  "Copy region as column (rectangle) to operating system's clipboard.
This command will also put the text in register 0.

See also: `kill-rectangle', `copy-to-register'."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (copy-rectangle-to-register ?0 p1 p2)
    (kill-new
     (with-temp-buffer
       (insert-register ?0)
       (buffer-string)))))
