;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      Color settings     ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(foreground-color . "white"))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Font height
(set-face-attribute 'default nil :height 120)

(custom-set-faces
 '(default ((t (:foreground "white" :background "black"))) t)
 '(text-cursor ((t (:foreground "black" :background "deeppink"))) t)
 '(pointer ((t (:foreground "magenta"))) t)
)

(provide 'appearance)
