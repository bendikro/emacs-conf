(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 (require 'reftex-cite)
			 (require 'bibtex)
			 (require 'flymake)
			 (require 'auto-complete-auctex)
			 ))


(defvar TeX-load-glsentries-regexp
  '("\\\\loadglsentries\\[[^]]*\\]{\\(\\.*[^#}%\\\\\\.\n\r]+\\)}"
	1 TeX-auto-file)
  "Matches \loadglsentries definitions.")

(add-hook
 'LaTeX-mode-hook
 '(lambda()
	(global-set-key (kbd "\t") 'TeX-complete-symbol) ; ' "fix" highlighting
	;; Completion for the bib-item entries in citen/citenp
	(setq TeX-complete-list
		  (append '(("\\\\citen{\\([^{}\n\r\\%,]*\\)"
					 1 LaTeX-bibitem-list "}")
					("\\\\citenp{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}"))
				  TeX-complete-list))
	;; Add litem as a valid item for e.g. enumerate environment. Fixed auto-fill (M-q)
	(LaTeX-paragraph-commands-add-locally (list "item" "litem" "ditem" "bitem" "initem" "newglossaryentry" "newacronym" "newdualentry"))

	(font-latex-add-keywords '(
							   ("citen" "[{")
							   ("cites" "[{")
							   ("code" "[{")
							   ("test" "[{")
							   ("name" "[{")
							   ("atodo" "[{")
							   ("feedback" "[{")
							   ("hostname" "[{")
							   ("host" "[{")
							   ("ms" "[{")
							   ("linuxkernel" "[{")
							   ("citenp" "[{")
							   ("citep" "[{")
							   ("citesp" "[{")
							   ("citerfc" "[{")
							   ("citerfcp" "[{")
							   ("cref" "[{")
							   ("Cref" "[{")
							   ("labelcref" "[{")
							   ("namecref" "[{")
							   ("newacronym" "[{")
							   ("newglossaryentry" "[{")
							   ("newdualentry" "[{")
							   ("gls" "[{")
							   ("Gls" "[{")
							   ("glspl" "[{")
							   ("Glspl" "[{")
							   ("glsname" "[{")
							   ("Glsname" "[{")
							   ("glstext" "[{")
							   ("Glstext" "[{")
							   ("glsuseri" "[{")
							   ("Glsuseri" "[{")
							   ("Glsed" "[{")
							   ("glsed" "[{")
							   ("Glsing" "[{")
							   ("glsing" "[{")
							   ("acrshort" "[{")
							   ("Acrshort" "[{")
							   ("acrshortpl" "[{")
							   ("Acrshortpl" "[{")
							   ("acrlong" "[{")
							   ("Acrlong" "[{")
							   ("acrlongpl" "[{")
							   ("Acrlongpl" "[{")
							   ("acrfull" "[{")
							   ("Acrfull" "[{")
							   ("litem" "[{")
							   ("ditem" "[{")
							   ("bitem" "[{")
							   ("initem" "[{")
							   ("captionof" "[{")
							   ("docaptionof" "[{")
							   ("erratumAdd" "[{")
							   ("erratumReplace" "[{{")
							   ("erratumDelete" "[{")
							   ("erratumNote" "[{")
							   ("noindent" "")
							   )
							 'reference)

	;; For regular macro completion
	(TeX-add-symbols
	 '("citen" ignore)
	 ;; Load glossary entries
	 '("citenp" ignore))))

(add-hook 'reftex-load-hook
		  '(lambda()
			 (setq reftex-bibliography-commands
				   '("addbibresource" "bibliography" "nobibliography"))))

(defun LaTeX-glossaries-auto-prepare ()
  "Prepare for LaTeX parsing."
  (setq TeX-auto-full-regexp-list
		(append (list TeX-load-glsentries-regexp)
				TeX-auto-full-regexp-list)))

(add-hook 'TeX-auto-prepare-hook 'LaTeX-glossaries-auto-prepare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activate RefTex and wire it into AucTex ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 'turn-on-reftex
			 'flyspell-mode nil
			 ;; Autosave before compiling
			 (setq TeX-save-query nil)
			 (setq TeX-show-compilation t)
			 ))

;(add-hook 'LaTeX-mode-hook 'text-mode-hook 'turn-on-auto-fill)
(provide 'setup-auctex)
