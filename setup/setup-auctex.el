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

	(font-latex-add-keywords '(
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
							   ("citep" "[{")
							   ("citesp" "[{")
							   ("citerfc" "[{")
							   ("citerfcp" "[{")
							   ("cref" "[{")
							   ("Cref" "[{")
							   ("labelcref" "[{")
							   ("namecref" "[{")
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
