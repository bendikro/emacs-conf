
(provide 'defaults)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
;; Delete selected text with del and CTRL-d
(delete-selection-mode t)

;; Disable shift selection (Shift+arrow keys)
(setq shift-select-mode nil)

;; Do not automatically add newline at end of file
(setq mode-require-final-newline nil)
;;(setq mode-require-final-newline 'ask)

;; By default, search is case-insensitive (Toggle with M-c when searching)
(setq case-fold-search t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;    Indentation    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Never insert tabs
;(set-default 'indent-tabs-mode nil)

;; Set tab width to 4 spaces
(setq-default tab-width 4)
(setq tab-width 4)



;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Show lines numbers on left side of text
(global-display-line-numbers-mode t)
(setq linum-format "%4d ")

;; Lines should be 80 characters wide, not 72
(setq-default fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
;(recentf-mode 0)
;;(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)


;; Show trailing white space
(setq-default show-trailing-whitespace t)
(setq-default show-ws-toggle-show-trailing-whitespace t)

;; Remove default text in new buffers
(setq initial-scratch-message "")

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words (Not supported in emacs < 24)
(if (> emacs-major-version 23)
	(global-subword-mode 1))

;; Don't break lines for me, please
;(setq-default truncate-lines t)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; vdiff: Enable show inline changes (highlight words that have changed in lines)
(setq vdiff-auto-refine t)

;; Enable editorconfig
(if (require 'editorconfig nil 'noerror)
	(editorconfig-mode 1)
  (message "editorconfig is not installed!"))

(if (require 'visual-regexp nil 'noerror)
	(message "Loaded visual-regexp")
  (message "visual-regexp is not installed!"))

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
