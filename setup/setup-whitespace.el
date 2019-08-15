(provide 'setup-whitespace)

(setq whitespace-display-mappings
  ;; all numbers are Unicode codepoint in decimal. ⁖ (insert-char 182 1)
  '(
;;;    (space-mark 32 [183] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
    ;(space-mark 32 [32] [46]) ; 32 SPACE 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
;;;    (newline-mark 10 [182 10]) ; 10 LINE FEED
;    (tab-mark 9 [9] [9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」

										;(space-mark   32    [183]     [ ])	; space
;;	(newline-mark   32    [183]     [32])	; space
;	(newline-mark   ?\n   [32 10])	; space

	;;(empty-newline-mark   ?\n   [10])	; space

;;	empty
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


(defun setup-go-whitespace ()
  (message "GO WHITESPACE")

;;  (setq whitespace-space-after-tab-regexp (" +\\(\\( \\{%d\\}\\)+\\)" . "\\( +\\) +"))

;;  (custom-set-variables '(whitespace-space-after-tab-regexp (" +\\(\\( \\{%d\\}\\)+\\)" . "\\( +\\) +")))
  
;;(setq whitespace-space-after-tab-regexp
;;		 (cons (concat tabs-class "+" "\\(" all-spaces-class "\\{%d,\\}\\)")
;;			   (concat whitespace-tab-regexp all-spaces-class "+")))

  (setq whitespace-style
		;;(quote (face trailing space-before-tab indentation empty space-after-tab ))
		(quote (face trailing space-before-tab indentation space-after-tab ))
		)

;; (set-face-attribute 'whitespace-empty nil
;;					  :foreground nil
;;					  )
;;
 (set-face-attribute 'whitespace-space-before-tab nil
					  :background "red"
					  )
;; (set-face-attribute 'whitespace-space-after-tab nil
;;					 :background "orange"
;;					  )

 
  (whitespace-mode 1)
)

;;(face tabs spaces trailing lines space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)


(add-hook 'go-mode-hook 'setup-go-whitespace)

;;(add-hook 'go-mode-hook 'whitespace-mode)
