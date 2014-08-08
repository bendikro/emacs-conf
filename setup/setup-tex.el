(provide 'setup-tex)
(add-to-list 'load-path "/home/bro/latex/auctex-lisp")
(ignore-errors (load "auctex.el" nil t t))

(require 'flymake)
(require 'auctex-latexmk)
(require 'auto-complete-auctex)
(require 'setup-latexmk)

(global-set-key (kbd "\t") 'TeX-complete-symbol) ; ' "fix" highlighting
(add-hook 'LaTeX-mode-hook 'auto-complete-mode t)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode-on)

(add-hook 'auto-complete-mode-hook
		  (define-key ac-completing-map "\e" 'ac-stop))

(defvar TeX-load-glsentries-regexp
  '("\\\\loadglsentries\\[[^]]*\\]{\\(\\.*[^#}%\\\\\\.\n\r]+\\)}"
	1 TeX-auto-file)
  "Matches \loadglsentries definitions.")

(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 (ac-set-trigger-key "TAB")
			 (setq ac-auto-start nil)
			 (message "ac-set-trigger-key")
			 (setq TeX-macro-default "/home/bro/master/master_opp/oppgave")
			 (setq TeX-auto-default "/home/bro/master/master_opp/oppgave/auto")
			 (TeX-auto-add-regexp TeX-load-glsentries-regexp)
			 (setq reftex-bibfile-ignore-list (list "rfc.bib"))
			 ))

(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 ;; Completion for the bib-item entries in citen/citenp
			 (setq TeX-complete-list
				   (skrot)
				   (append '(("\\\\citen{\\([^{}\n\r\\%,]*\\)"
							  1 LaTeX-bibitem-list "}")
							 ("\\\\citenp{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}"))
						   TeX-complete-list))

			 (setq font-latex-match-reference-keywords
				   '(
					 ("citen" "[{")
					 ("citenp" "[{")
					 ("gls" "[{")
					 ("Gls" "[{")))
			 ;; For regular macro completion
			 (TeX-add-symbols
			  '("citen" ignore)
			  ;; Load glossary entries
			  '("citenp" ignore))))


(defun LaTeX-glossaries-auto-prepare ()
  "Prepare for LaTeX parsing."
  (setq TeX-auto-full-regexp-list
		(append (list TeX-load-glsentries-regexp)
				TeX-auto-full-regexp-list)))

(add-hook 'TeX-auto-prepare-hook 'LaTeX-glossaries-auto-prepare)

;; Split window and show messages
(messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activate RefTex and wire it into AucTex ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(add-hook 'LaTeX-mode-hook 'flyspell-mode nil)
;(add-hook 'LaTeX-mode-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook '(lambda()
							 (set-fill-column 80)
							 (setq TeX-error-overview-open-after-TeX-run t)
							 ))

;; Autosave before compiling
(setq TeX-save-query nil)

(setq TeX-show-compilation nil)
