(provide 'setup-auctex)

(load "auctex.el" nil t t)

(require 'reftex-cite)
(require 'bibtex)
(require 'flymake)
(require 'auto-complete-auctex)

(defvar TeX-load-glsentries-regexp
  '("\\\\loadglsentries\\[[^]]*\\]{\\(\\.*[^#}%\\\\\\.\n\r]+\\)}"
	1 TeX-auto-file)
  "Matches \loadglsentries definitions.")

(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 (global-set-key (kbd "\t") 'TeX-complete-symbol) ; ' "fix" highlighting
			 ))

;; Custom stuff to load glossaries automatically and ignore rfc bib
(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 (setq TeX-macro-default "/home/bro/master/master_opp/oppgave")
			 (setq TeX-auto-default "/home/bro/master/master_opp/oppgave/auto")
			 (TeX-auto-add-regexp TeX-load-glsentries-regexp)
			 (setq reftex-bibfile-ignore-list (list "rfc.bib"))))

(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 ;; Completion for the bib-item entries in citen/citenp
			 (setq TeX-complete-list
				   (append '(("\\\\citen{\\([^{}\n\r\\%,]*\\)"
							  1 LaTeX-bibitem-list "}")
							 ("\\\\citenp{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}"))
						   TeX-complete-list))

			 (setq font-latex-match-reference-keywords
				   '(
					 ("citen" "[{")
					 ("cites" "[{")
					 ("code" "[{")
					 ("name" "[{")
					 ("hostname" "[{")
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
					 ))

			 ;; Add litem as a valid item for e.g. enumerate environment. Fixed auto-fill (M-q)
			 (LaTeX-paragraph-commands-add-locally (list "item" "litem" "ditem" "bitem" "initem" "newglossaryentry" "newacronym" "newdualentry"))

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
