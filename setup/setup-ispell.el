(provide 'setup-ispell)

(require 'ispell)

;((("\\\\addcontentsline" ispell-tex-arg-end 2)
;  ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end)
;  ("\\\\\\([aA]lph\\|arabic\\)" ispell-tex-arg-end)
;  ("\\\\bibliographystyle" ispell-tex-arg-end)
;  ("\\\\makebox" ispell-tex-arg-end 0)
;  ("\\\\e?psfig" ispell-tex-arg-end)
;  ("\\\\document\\(class\\|style\\)" . "\\\\begin[ 	\n]*{[ 	\n]*document[ 	\n]*}"))
; (("\\(figure\\|table\\)\\*?" ispell-tex-arg-end 0)
;  ("list" ispell-tex-arg-end 2)
;  ("program" . "\\\\end[ 	\n]*{[ 	\n]*program[ 	\n]*}")
;  ("verbatim\\*?" . "\\\\end[ 	\n]*{[ 	\n]*verbatim\\*?[ 	\n]*}")))



;(defun LaTeX-ispell-skip-environment2 (env &optional star)
;  (let ((start (concat "%s" (if star "\\*?")))
;    (end   (concat "\\\\end[    \n]*{[  \n]*%s" (if star "\\*?") "[     \n]*}")))
;    (let ((env-list (cddadr ispell-tex-skip-alists)))
;      (setcdr (last env-list)
;              (list 
;               (cons (format start env)
;                     (format end env)))))))
;
;(defun LaTeX-ispell-skip-environment (env &optional star)
;  (let ((start (concat "%s" (if star "\\*?")))
;    (end   (concat "\\\\end[    \n]*{[  \n]*%s" (if star "\\*?") "[     \n]*}")))
;    (let ((env-list (cddadr ispell-tex-skip-alists)))
;      (nconc env-list
;             (list
;              (cons (format start env)
;                    (format end env)))))))
;
;(LaTeX-ispell-skip-environment "env3")
;
;(setq ispell-tex-skip-alists
;      (list
;       (append
;		(car ispell-tex-skip-alists)
;		'(
;		  ("\\\\mycommad"       ispell-tex-arg-end)
;		  ("\\\\mycommadtwo"    ispell-tex-arg-end 2)
;		  ("\\\\feedback" ispell-tex-arg-end)
;		  ("\\\\gls" ispell-tex-arg-end)
;		  ("\\\\gls" ispell-tex-arg-end)
;		  ))
;       (cadr ispell-tex-skip-alists)))


(setq ispell-tex-skip-alists
  '((("%\\[" . "%\\]")
     ;; All the standard LaTeX keywords from L. Lamport's guide:
     ;; \cite, \hspace, \hspace*, \hyphenation, \include, \includeonly, \input,
     ;; \label, \nocite, \rule (in ispell - rest included here)
     ("\\\\addcontentsline"              ispell-tex-arg-end 2)
     ("\\\\add\\(tocontents\\|vspace\\)" ispell-tex-arg-end)
     ("\\\\\\([aA]lph\\|arabic\\)" ispell-tex-arg-end)
     ("\\\\author" ispell-tex-arg-end)
     ("\\\\bibliographystyle" ispell-tex-arg-end)
     ("\\\\makebox" ispell-tex-arg-end 0)
     ;;("\\\\epsfig"ispell-tex-arg-end)
     ("\\\\document\\(class\\|style\\)" .
      "\\\\begin[ \t\n]*{[ \t\n]*document[ \t\n]*}")
	 ;;
	 ("\\\\feedback" ispell-tex-arg-end)
	 ("\\\\gls" ispell-tex-arg-end)
	 ("\\\\gls" ispell-tex-arg-end)
	 )
    (;; delimited with \begin.  In ispell: displaymath, eqnarray, eqnarray*,
     ;; equation, minipage, picture, tabular, tabular* (ispell)
     ("\\(figure\\|table\\)\\*?"  ispell-tex-arg-end 0)
     ("list"  ispell-tex-arg-end 2)
     ("program". "\\\\end[ \t\n]*{[ \t\n]*program[ \t\n]*}")
     ("verbatim\\*?". "\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}")
     ("gather\\*?". "\\\\end[ \t\n]*{[ \t\n]*gather\\*?[ \t\n]*}")
;     ("\\(ccode\\|tablebox\\|command\\)\\*?"  ispell-tex-arg-end )
     ("\\(ccode\\|tablebox\\|command\\|pseudocode\\)\\*?". "\\\\end[ \t\n]*{[ \t\n]*.*?\\*?[ \t\n]*}")
	 ))
)
