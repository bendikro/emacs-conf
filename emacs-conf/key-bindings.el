(provide 'key-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;          Key bindings             ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-key-bindings ()
  (interactive)
  (progn
	;; Fix delete character
	(global-set-key [delete] 'delete-char)

	;; -------------------
	;; Make ALT-backspace and ALT-del delete words without copying text til kill-ring

	;; Must undefine and redefine M-delete/M-backspace to work in windowed mode
	(define-key input-decode-map [M-delete] nil)
	(define-key input-decode-map [M-backspace] nil)

	;; ALT-del
	(define-key input-decode-map "\e[3;3~" [M-del]) ; Make it works in tmux and screen
	(global-set-key [M-del] 'delete-word-no-copy) ;; Required for no-window mode
	(global-set-key (kbd "M-<delete>") 'delete-word-no-copy)  ;; Required for window mode

	;; ALT-backspace delete word backwards without copy
	(global-set-key (kbd "M-<DEL>") 'backward-delete-word-no-copy)

	;; CTRL-del delete word
	(define-key input-decode-map "\e[3;5~" [C-del]) ; Make in work in tmux and screen
	(global-set-key [C-del] 'kill-word)
	;; -------------------

	;; Bind Meta-arrow up/down to scroll without moving cursor
	(global-set-key [M-up] 'scroll-down-keep-cursor)
	(global-set-key [M-down] 'scroll-up-keep-cursor)

	;; Bind Meta-arrow left/right to beginning/end of line
	(global-set-key [M-right] 'move-end-of-line)
	(global-set-key [M-left]  'move-beginning-of-line)

	;; Bind Meta-arrow left/right to beginning/end of line
	(global-set-key [S-right] 'forward-whitespace)
	(global-set-key [S-left]  'backward-whitespace)

	;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; ;; Fix for screen / tmux

	;; Fix CTRL + arrow keys inside screen/tmux
	(define-key input-decode-map "\e[1;5A" [C-up])
	(define-key input-decode-map "\e[1;5B" [C-down])
	(define-key input-decode-map "\e[1;5C" [C-right])
	(define-key input-decode-map "\e[1;5D" [C-left])

	;; Fix ALT + arrow keys inside screen/tmux
	(define-key input-decode-map "\e[1;3A" [M-up])
	(define-key input-decode-map "\e[1;3B" [M-down])
	(define-key input-decode-map "\e[1;3C" [M-right])
	(define-key input-decode-map "\e[1;3D" [M-left])

	;; Fix End key
	(global-set-key [select]  'move-end-of-line)

	;; ;;;;;;;;;;;;;;;
	;; ;; Misc
	;; ;;;;;;;;;;;;;;;

	;; Move to beginning/end of buffer
	(global-set-key (kbd "C-c C-b") 'beginning-of-buffer)
	(global-set-key (kbd "C-c C-e") 'end-of-buffer)

	;; These are disabled by default, so enable
	(put 'upcase-region 'disabled nil)
	(put 'downcase-region 'disabled nil)

;; Rectangle settings
; (global-set-key [f5] 'copy-region-as-kill)   ; Copy
; (global-set-key [f6] 'kill-rectangle)        ; Cut
; (global-set-key [f7] 'yank-rectangle)        ; Paste

	;; Refresh file (read from disk) wit F5
	(global-set-key [f5] 'revert-buffer-keep-undo)

	;; Toggle between 4 and 8 character tab width
	(global-set-key (kbd "<f8>") 'tf-toggle-tab-width-setting)

	(global-set-key (kbd "M-#") 'replace-regexp)

	(global-set-key (kbd "S-<kp-8>") 'shrink-window)
	(global-set-key (kbd "S-<kp-2>") 'enlarge-window)
	(global-set-key (kbd "S-<kp-4>") 'shrink-window-horizontally)
	(global-set-key (kbd "S-<kp-6>") 'enlarge-window-horizontally)

;;(global-set-key (kbd "<kp-1>") 'recentf-open-files) ; numberic keypad 1
;;(global-set-key (kbd "<kp-2>") 'bookmark-bmenu-list)
;;(global-set-key (kbd "<kp-3>") 'ibuffer)

	(global-set-key (kbd "C-c r") #'resize-mode)

	;; Bind keys to move cursor between windows
	(global-set-key (kbd "C-c <C-left>") (ignore-error-wrapper 'windmove-left))
	(global-set-key (kbd "C-c <C-right>") (ignore-error-wrapper 'windmove-right))
	(global-set-key (kbd "C-c <C-up>") (ignore-error-wrapper 'windmove-up))
	(global-set-key (kbd "C-c <C-down>") (ignore-error-wrapper 'windmove-down))

	;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Mode specific bindings
	;;;;;;;;;;;;;;;;;;;;;;;;;;
	(add-hook 'python-mode-hook
			  '(lambda() (global-set-key (kbd "C-c f") #'flycheck-now-mode)))

	;; Handle key bindings for following compile buffer
	(add-hook 'TeX-mode-hook
			  '(lambda()
				 (local-unset-key (kbd "C-c C-f"))
				 (local-set-key (kbd "C-c C-f") 'follow-auctex-compile-buffer)
				 (local-set-key (kbd "C-c C-g") #'encapsulate-glossary)))
	))

(add-hook 'after-init-load-hook
		  (lambda ()
			(do-key-bindings)))


;; keys to set for programming modes
(defun set-programming-keys ()
  (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-line-or-region)
)

(add-hooks 'set-programming-keys
		   '(python-mode-hook
			 c-mode-hook
			 go-mode-hook

			 makefile-mode-hook
			 sh-mode-hook
			 yaml-mode-hook
			 css-mode-hook
			 conf-mode-hook
			 jinja2-mode-hook
			 rockerfile-mode-hook
			 latex-mode-hook
			 ))
