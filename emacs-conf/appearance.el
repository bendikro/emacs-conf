(provide 'appearance)

;; initial window
(setq initial-frame-alist
      '(
        (width . 132) ; character
        (height . 54) ; lines
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      Color settings     ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Font height
(set-face-attribute 'default nil :height 120)

(add-hook 'after-init-load-hook
		  (require-safe "color-theme-bro.el"))
