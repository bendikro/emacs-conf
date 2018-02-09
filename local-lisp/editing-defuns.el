;; Basic text editing defuns

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;    Misc functions                       ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;; Count Lines, words and characters in a region
;;;;;;; Could use count-words-region (M-=), but don't like the output format

(defun count-region (posBegin posEnd)
  "Print number of Lines, words and chars in region."
  (interactive "r")
  (message "Counting …")
  (save-excursion
    (let (wordCount charCount)
      (setq wordCount 0)
      (setq lineCount 0)
      (setq charCount (- posEnd posBegin))
      (goto-char posBegin)
      (while (and (< (point) posEnd)
                  (re-search-forward "\\w+\\W*" posEnd t))
        (setq wordCount (1+ wordCount)))
      (goto-char posBegin)
      (while (and (< (point) posEnd)
                  (re-search-forward "\n" posEnd t))
        (setq lineCount (1+ lineCount)))
      (message "Lines: %d Words: %d Chars: %d" lineCount wordCount charCount)
      )))

(defun revert-buffer-keep-undo (&rest -)
  "Revert buffer but keep undo history."
  (interactive)
  ;; Get cursor position
  (setq cursor-pos (point))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents (buffer-file-name))
    (set-visited-file-modtime (visited-file-modtime))
    (set-buffer-modified-p nil))
  ;; Set cursor position
  (goto-char cursor-pos))

;(setq revert-buffer-function 'revert-buffer-keep-undo)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

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

(provide 'editing-defuns)
