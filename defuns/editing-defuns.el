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
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-file-contents (buffer-file-name))
    (set-visited-file-modtime (visited-file-modtime))
    (set-buffer-modified-p nil)))

;(setq revert-buffer-function 'revert-buffer-keep-undo)

;(provide 'file-defuns)