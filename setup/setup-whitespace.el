(provide 'setup-whitespace)
(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
  '(
;;    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    (space-mark 32 [32] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
;    (newline-mark 10 [182 10]) ; 10 LINE FEED
    (tab-mark 9 [9] [9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
    ))

;;(custom-set-faces
;;  '(whitespace-space ((t (:bold t :foreground "gray75"))))
;;  '(whitespace-empty ((t (:foreground "firebrick" :background "SlateGray1"))))
;;  '(whitespace-hspace ((t (:foreground "lightgray" :background "LemonChiffon3"))))
;;  '(whitespace-indentation ((t (:foreground "firebrick" :background "blue"))))
;;;;  '(whitespace-line ((t (:foreground "black" :background "red")))) ;; Long lines
;;  '(whitespace-newline ((t (:foreground "orange" :background "blue"))))
;;  '(whitespace-space-after-tab ((t (:foreground "black" :background "green"))))
;;  '(whitespace-space-before-tab ((t (:foreground "black" :background "DarkOrange"))))
;;  '(whitespace-tab ((t (:foreground "blue" :background "green"))))
;;  '(whitespace-trailing ((t (:foreground "red" :background "yellow"))))
;;  )
