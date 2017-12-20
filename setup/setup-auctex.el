(add-hook 'TeX-mode-hook
		  '(lambda()
			 (require 'reftex-cite)
			 (require 'bibtex)
			 (require 'flymake)
			 (require 'auto-complete-auctex)
			 ))

(add-hook
 'TeX-mode-hook
 '(lambda()
	(global-set-key (kbd "\t") 'TeX-complete-symbol) ; ' "fix" highlighting
	;; Completion for the bib-item entries in citen/citenp
	(setq TeX-complete-list
		  (append '(("\\\\citep{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}")
					;;("\\\\citenp{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}")
					)
				  TeX-complete-list))

	;; For regular macro completion
	(TeX-add-symbols
	 '("citep" TeX-arg-cite))

	;; Add litem as a valid item for e.g. enumerate environment. Fixed auto-fill (M-q)
	(LaTeX-paragraph-commands-add-locally (list "item" "litem" "ditem" "bitem" "initem" "newglossaryentry" "newacronym" "newdualentry"))

	;; General
	(font-latex-add-keywords '(
							   ("NeedsTeXFormat" "[{")
							   ("ProvidesPackage" "[{")
							   ("RequirePackage" "[{")
							   ("noindent" "")
							   ("def" "")
							   ("protected" "")
							   ("expandonce" "")
							   )
							 'reference)

	;; cleveref
	(font-latex-add-keywords '(
							   ("cref" "[{")
							   ("Cref" "[{")
							   ("labelcref" "[{")
							   ("namecref" "[{")
							   )
							 'reference)

	;; caption
	(font-latex-add-keywords '(
							   ("captionof" "[{")
							   )
							 'reference)


	;; kvoptions
	(font-latex-add-keywords '(
							   ("SetupKeyvalOptions" "[{")
							   ("DeclareStringOption" "[{")
							   ("DeclareBoolOption" "[{")
							   ("ProcessKeyvalOptions" "[{")
							   )
							 'reference)

	;; xparse
	(font-latex-add-keywords '(
							   ("NewDocumentCommand" "[{")
							   ("DeclareExpandableDocumentCommand" "[{")
							   )
							 'reference)

	;; etoolbox
	(font-latex-add-keywords '(
							   ("ifboolexpr" "[{")
							   ("protected@csxdef" "[{")
							   )
							 'reference)

	;; xinttools
	(font-latex-add-keywords '(
							   ("xintFor" "[{")
							   )
							 'reference)

	;; xstring
	(font-latex-add-keywords '(
							   ("StrGobbleLeft" "[{")
							   )
							 'reference)

	;; ifthenelse
	(font-latex-add-keywords '(
							   ("ifthenelse" "[{")
							   )
							 'reference)



	;; pgfkeys
	(font-latex-add-keywords '(
							   ("pgfkeys" "[{")
							   ("pgfkeysdefnargs" "[{")
							   ("pgfkeysdefargs" "[{")
							   ("pgfkeyscurrentpath" "[{")
							   ("pgfkeysgetvalue" "[{")
							   ("pgfkeyssetvalue" "[{")
							   ("pgfkeysdefaultpath" "[{")
							   ("pgfqkeys" "[{")
							   ("pgfqkeysalso" "[{")
							   ("pgfkeysvalueof" "[{")
							   ("pgfkeysifdefined" "[{")
							   )
							 'reference)

	;; Errata2
	(font-latex-add-keywords '(
							   ("erratumAdd" "[{{")
							   ("erratumReplace" "[{{{")
							   ("erratumDelete" "[{{")
							   ("erratumNote" "[{{")
							   )
							 'reference)

	;; Master
	(font-latex-add-keywords '(
							   ("linuxkernel" "[{")
							   ("citep" "[{")
							   ("citesp" "[{")
							   ("citerfc" "[{")
							   ("citerfcp" "[{")
							   ("cites" "[{")
							   ("code" "[{")
							   ("atodo" "[{")
							   ("feedback" "[{")
							   ("hostname" "[{")
							   ("name" "[{")
							   ("test" "[{")
							   ("host" "[{")
							   ("ms" "[{")

							   ("litem" "[{")
							   ("ditem" "[{")
							   ("bitem" "[{")
							   ("initem" "[{")

							   ("docaptionof" "[{")
							   )
							 'reference)

	))



(add-hook 'reftex-load-hook
		  '(lambda()
			 (setq reftex-bibliography-commands
				   '("addbibresource" "bibliography" "nobibliography"))))

(defun TeX-glossaries-auto-prepare ()
  "Prepare for LaTeX parsing."
  (message "TeX-glossaries-auto-prepare")
  ;; Load glossaries style to define
  ;;(load-file "~/.emacs.d/auctex/style/glossaries.el")
  ;;(TeX-load-style "glossaries")
)

;; This hook is run for each tex file that doesn't already have an <name>.el file in auto/ dir
(add-hook 'TeX-auto-prepare-hook 'TeX-glossaries-auto-prepare)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activate RefTex and wire it into AucTex ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'TeX-mode-hook
		  '(lambda()
			 'turn-on-reftex
			 'flyspell-mode nil
			 ;; Autosave before compiling
			 (setq TeX-save-query nil)
			 (setq TeX-show-compilation t)
			 (TeX-load-style "glossaries")
			 (init-LaTeX-glossaries-style)
			 ))

(provide 'setup-auctex)
