(provide 'key-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;          Key bindings             ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-key-bindings ()
  (interactive)
  (progn
	;; Fix delete character
	(global-set-key [delete] 'delete-char)

	;; Make ALT-backspace and ALT-del delete words without copying text til kill-ring
	(define-key input-decode-map "\e[3;3~" [M-del]) ; Make in work in tmux and screen
	(global-set-key [M-del] 'delete-word-no-copy)

	;; CTRL-del delete word
	(define-key input-decode-map "\e[3;5~" [C-del]) ; Make in work in tmux and screen
	(global-set-key [C-del] 'kill-word)

	;; ALT-backspace delete word backwards
	(global-set-key [?\M-\d] 'backward-delete-word-no-copy)

	;; Bind Meta-arrow up/down to scroll without moving cursor
	(global-set-key [M-up] 'scroll-down-keep-cursor)
	(global-set-key [M-down] 'scroll-up-keep-cursor)

	;; Bind Meta-arrow left/right to beginning/end of line
	(global-set-key [M-right] 'move-end-of-line)
	(global-set-key [M-left]  'move-beginning-of-line)

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

	;; ;;;;;;;;;;;;;;;
	;; ;; Misc
	;; ;;;;;;;;;;;;;;;

	;;(require 'cc-mode)
	;; comment / uncomment region
	(global-set-key "\C-cc" 'comment-region)
	(global-set-key "\C-cu" 'uncomment-region)

;; Rectangle settings
; (global-set-key [f5] 'copy-region-as-kill)   ; Copy
; (global-set-key [f6] 'kill-rectangle)        ; Cut
; (global-set-key [f7] 'yank-rectangle)        ; Paste

	;; Refresh file (read from disk) wit F5
	(global-set-key [f5] 'revert-buffer-keep-undo)

	;; Toggle between 4 and 8 character tab width
	(global-set-key (kbd "<f8>") 'tf-toggle-tab-width-setting) ; ' "fix" highlighting

	;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; ;;;;;;; Key bindings for gtag
	;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(global-set-key "\M-," 'ww-next-gtag)      ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
	(global-set-key "\M-*" 'gtags-pop-stack)   ;; M-, cycles to next result, after doing M-. C-M-. or C-M-,
	(global-set-key "\M-." 'gtags-find-tag2)    ;; M-. finds tag
;;(global-set-key [(control meta .)] 'gtags-find-rtag)   ;; C-M-. find all references of tag
;;(global-set-key [(control meta ,)] 'gtags-find-symbol) ;; C-M-, find all usages of symbol.

	(global-set-key "\M--" 'gtags-find-rtag)    ;; M-- finds tag references

	;; Update TAGS file with F9
	(global-set-key (kbd "<f9>") 'gtags-update-current-file)

;;(global-set-key (kbd "<C-M-up>") 'shrink-window)
;;(global-set-key (kbd "<C-M-down>") 'enlarge-window)
;;(global-set-key (kbd "<C-M-left>") 'shrink-window-horizontally)
;;(global-set-key (kbd "<C-M-right>") 'enlarge-window-horizontally)

; (define-key local-function-key-map [M-kp-2] [?\C-2])

	(global-set-key (kbd "S-<kp-8>") 'shrink-window)
	(global-set-key (kbd "S-<kp-2>") 'enlarge-window)
	(global-set-key (kbd "S-<kp-4>") 'shrink-window-horizontally)
	(global-set-key (kbd "S-<kp-6>") 'enlarge-window-horizontally)

;;(global-set-key (kbd "<kp-1>") 'recentf-open-files) ; numberic keypad 1
;;(global-set-key (kbd "<kp-2>") 'bookmark-bmenu-list)
;;(global-set-key (kbd "<kp-3>") 'ibuffer)

	(global-set-key (kbd "C-c r") #'resize-mode)
	(global-set-key (kbd "C-c f") #'flycheck-now-mode)
	(local-unset-key (kbd "C-c C-f"))
	(global-set-key (kbd "C-c C-f") 'follow-auctex-compile-buffer)
	))

(add-hook 'after-init-load-hook
		  (lambda ()
			(do-key-bindings)))
