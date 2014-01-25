(provide 'key-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;          Key bindings             ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix delete character
(global-set-key [delete] 'delete-char)

;; Make ALT-backspace and ALT-del delete words without copying text til kill-ring
(global-set-key (read-kbd-macro "<M-delete>") 'delete-word-no-copy) ; ALT-del
(global-set-key "\M-\d" 'backward-delete-word-no-copy)              ; ALT-backspace

;(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

; This is for CTRL-del and ALT-del inside screen
(global-set-key "\M-[3;5~" 'kill-word)           ; CTRL-del
(global-set-key "\M-[3;3~" 'delete-word-no-copy) ; ALT-del

;; Fix CTRL + arrow keys inside screen
(global-set-key "\M-[1;5A"    'backward-paragraph)   ; Ctrl + up
(global-set-key "\M-[1;5B"    'forward-paragraph)    ; Ctrl + down
(global-set-key "\M-[1;5C"    'forward-word)         ; Ctrl + right
(global-set-key "\M-[1;5D"    'backward-word)        ; Ctrl + left

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll up/down and keep cursor position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Scroll the text one line down while keeping the cursor
(defun scroll-down-keep-cursor ()
   (interactive)
   (scroll-down 1))

;; Scroll the text one line up while keeping the cursor
(defun scroll-up-keep-cursor ()
   (interactive)
   (scroll-up 1))

;; Bind Meta-arrow up/down to scroll without moving cursor
(global-set-key [M-up] 'scroll-down-keep-cursor)
(global-set-key [M-down] 'scroll-up-keep-cursor)

;; Bind Meta-arrow left/right to beginning/end of line
(global-set-key [M-right] 'move-end-of-line)
(global-set-key [M-left]  'move-beginning-of-line)

;; This is for ALT + up/down/right/left to work when running inside screen
(global-set-key "\M-[1;3A"    'scroll-down-keep-cursor)    ; Alt + up
(global-set-key "\M-[1;3B"    'scroll-up-keep-cursor)      ; Alt + down
(global-set-key "\M-[1;3C"    'move-end-of-line)           ; Alt+right
(global-set-key "\M-[1;3D"    'move-beginning-of-line)     ; Alt+left

;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;

;;(require 'cc-mode)
;; comment / uncomment region
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

(define-prefix-command 'flycheck-map)
(global-set-key (kbd "C-f") 'flycheck-map)
(define-key flycheck-map (kbd "C-n") 'flycheck-next-error)
(define-key flycheck-map (kbd "C-p") 'flycheck-previous-error)

;(global-set-key "\C-fn" 'flycheck-next-error)
;(global-set-key "\C-fp" 'flycheck-previous-error)

;; Rectangle settings
; (global-set-key [f5] 'copy-region-as-kill)   ; Copy
; (global-set-key [f6] 'kill-rectangle)        ; Cut
; (global-set-key [f7] 'yank-rectangle)        ; Paste

;; Refresh file (read from disk) wit F5
(global-set-key [f5] 'revert-buffer-keep-undo)

;; Toggle between 4 and 8 character tab width
(global-set-key (kbd "<f8>") 'tf-toggle-tab-width-setting) ; ' "fix" highlighting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; Key bindings for gtag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\M-," 'ww-next-gtag)      ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
(global-set-key "\M-*" 'gtags-pop-stack)   ;; M-, cycles to next result, after doing M-. C-M-. or C-M-,
(global-set-key "\M-." 'gtags-find-tag2)    ;; M-. finds tag
;(global-set-key [(control meta .)] 'gtags-find-rtag)   ;; C-M-. find all references of tag
;(global-set-key [(control meta ,)] 'gtags-find-symbol) ;; C-M-, find all usages of symbol.

(global-set-key "\M--" 'gtags-find-rtag)    ;; M-- finds tag references

;; Update TAGS file with F9
(global-set-key (kbd "<f9>") 'gtags-update-current-file)
