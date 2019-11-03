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

;; Set custom theme bro
(setq frame-background-mode 'dark)
(setq custom-safe-themes t)

;; Add emacs-conf/themes to load path
(add-to-list 'custom-theme-load-path
			 (concat (file-name-as-directory user-home-dir) ".emacs.d/emacs-conf/themes"))

(add-hook 'after-init-load-hook
		  (lambda ()
			(load-theme 'bro t)
			))
