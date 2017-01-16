;;; glossaries.el --- AUCTeX style for `glossaries.sty'

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: Bendik Rønning Opstad <bro.development@gmail.com>
;; Maintainer: auctex-devel@gnu.org
;; Keywords: tex

;; This file is part of AUCTeX.

;; AUCTeX is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; AUCTeX is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with AUCTeX; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Code:

(require 'tex)

(TeX-auto-add-type "glossaries-acronym" "LaTeX")
(TeX-auto-add-type "glossaries" "LaTeX")

;; Tell AUCTeX that \loadglsentries loads a file.  regexp is the same as
;; for \input or \include.  This will run `TeX-run-style-hooks' on
;; subfile(s) when master file is loaded.
(defvar TeX-load-glsentries-regexp
  '("\\\\loadglsentries\\[[^]]*\\]{\\(\\.*[^#}%\\\\\\.\n\r]+\\)}"
	1 TeX-auto-file)
  "Matches \loadglsentries definitions. The file containing this command
will have first argument to the command added to the call to TeX-run-style-hooks
in its auto/<file>.el file")

(defvar TeX-newacronym-regexp
  '("\\\\newacronym{\\([^}{]*\\)}{\\([^}{]*\\)}{\\([^}{]*\\)}"
    (1 2 3) LaTeX-auto-glossaries-acronym)
  "Matches \newacronym definitions.")

(defvar TeX-newglossary-regexp
  '("\\\\newglossaryentry{\\([^}{]*\\)}"
	(1 1) LaTeX-auto-glossaries)
  "Matches \newglossaryentry definitions.")

;;(defvar LaTeX-auto-glossaries nil
;;  "Temporary for parsing glossaries by `glossaries' package.")

;;(defvar LaTeX-auto-glossaries-acronym nil
;;  "Temporary for parsing acronyms by `glossaries' package.")

;; The *-list variables are local to each buffer, so need to use global variables
(defvar LaTeX-glossaries-list-global nil
  "Global glossaries list")

(defvar LaTeX-glossaries-acronym-list-global nil
  "Global acronym list")

(defun LaTeX-glossaries-cleanup ()
  "Move acronyms from `LaTeX-auto-glossaries' to `LaTeX-glossaries-list'."
  (mapc (lambda (glsentry)
		  (add-to-list 'LaTeX-glossaries-list glsentry))
		LaTeX-auto-glossaries)

  (mapc (lambda (glsentry)
		  (add-to-list 'LaTeX-glossaries-list-global glsentry))
		LaTeX-auto-glossaries)

  (mapc (lambda (acronym)
		  (add-to-list 'LaTeX-glossaries-acronym-list acronym))
		LaTeX-auto-glossaries-acronym)

  (mapc (lambda (acronym)
		  (add-to-list 'LaTeX-glossaries-acronym-list-global acronym))
		LaTeX-auto-glossaries-acronym)
)

(defun LaTeX-acronym-cleanup ()
  "Move acronyms from `LaTeX-auto-acronym' to `LaTeX-acronym-list'."
  (mapc (lambda (acronym)
		  (add-to-list 'LaTeX-acronym-list (list acronym)))
		LaTeX-auto-acronym))


(defun LaTeX-glossaries-prepare ()
  " Clear `LaTeX-auto-glossaries-glsentries' before use."
  (setq LaTeX-auto-glossaries nil)
  (setq LaTeX-auto-glossaries-acronym nil)
)

;; Called for each new file that is opened
(add-hook 'TeX-auto-prepare-hook #'LaTeX-glossaries-prepare t)
(add-hook 'TeX-auto-cleanup-hook #'LaTeX-glossaries-cleanup t)
(add-hook 'TeX-update-style-hook #'TeX-auto-parse t)

(defvar LaTeX-glossaries-acronym-history nil
  "History of acronyms in acronym.")

(defvar LaTeX-glossaries-history nil
  "History of acronyms in acronym.")

(defun LaTeX-arg-glossaries-glsentry (optional &optional prompt definition)
  "Prompt for a gls-entry completing with known gls-entries.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen gls-entry to the
list of defined gls-entries."
  ;; Copy from global to local list because global list will give "void funtion" error....???
  (mapc (lambda (glsentry)
		  (add-to-list 'LaTeX-glossaries-list glsentry))
		LaTeX-glossaries-list-global)

  (let ((glossary
		 (completing-read (TeX-argument-prompt optional prompt "Glossary")
						  (LaTeX-glossaries-list) nil nil nil
						  'LaTeX-glossaries-history)))
    (if (and definition (not (string-equal "" glossary)))
		(LaTeX-add-glossariess glossary)) ; Extra (plural) s on "glossariess" added by TeX-auto-add-type
    (TeX-argument-insert glossary optional optional)))

(defun LaTeX-arg-define-glossaries-glsentry (optional &optional prompt)
  "Prompt for a gls-entry completing with known gls-entries.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (LaTeX-arg-glossaries-glsentry optional prompt t))

(defun LaTeX-arg-glossaries-acronym (optional &optional prompt definition)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string.  If DEFINITION is non-nil, add the chosen acronym to the
list of defined acronyms."
  ;; Copy from global to local list because global list will give "void funtion" error....???
  (mapc (lambda (acrentry)
		  (add-to-list 'LaTeX-glossaries-acronym-list acrentry))
		LaTeX-glossaries-acronym-list-global)

  (let ((acronym
		 (completing-read (TeX-argument-prompt optional prompt "Acronym")
						  (LaTeX-glossaries-acronym-list) nil nil nil
						  'LaTeX-glossaries-acronym-history)))
    (if (and definition (not (string-equal "" acronym)))
		(LaTeX-add-glossaries-acronymss acronym)) ; Extra (plural) s on "acronymss" added by TeX-auto-add-type
    (TeX-argument-insert acronym optional optional)))

(defun LaTeX-arg-define-glossaries-acronym (optional &optional prompt)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (LaTeX-arg-glossaries-acronym optional prompt t))

(defvar LaTeX-glossaries-package-options
  '("toc" "acronym" "nopostdot" "xindy" "nonumberlist" "noredefwarn" "shortcuts")
  "Package options for the glossaries package.")


(TeX-add-style-hook "glossaries"
 (function
  (lambda ()
	(message "MSG Loading AUCTeX glossaries style")

	(TeX-auto-add-regexp TeX-newacronym-regexp)
	(TeX-auto-add-regexp TeX-newglossary-regexp)
	(TeX-auto-add-regexp TeX-load-glsentries-regexp)

	(LaTeX-add-environments
	 '("glossaries" LaTeX-env-args
	   [TeX-arg-string "Longest glossary"]))

	;; Are these required for anything?
    ;;;; Completion for the glossary entries in \gls
	;;(setq TeX-complete-list
	;;	  (append '(("\\gls{\\([^{}\n\r\\%,]*\\)"
	;;				 1 LaTeX-glossaries-list-global "}")
	;;				("\\Gls{\\([^{}\n\r\\%,]*\\)"
	;;				 1 LaTeX-glossaries-list-global "}"))
	;;			  TeX-complete-list))
	;;
	;;;; Completion for the glossary entries in \acr
	;;(setq TeX-complete-list
	;;	  (append '(("\\acrshort{\\([^{}\n\r\\%,]*\\)"
	;;				 1 LaTeX-glossaries-acronym-list "}")
	;;				("\\acrfull{\\([^{}\n\r\\%,]*\\)"
	;;				 1 LaTeX-glossaries-acronym-list "}"))
	;;			  TeX-complete-list))

    ;; RefTeX support
    (and (fboundp 'reftex-add-index-macros)
		 (reftex-add-index-macros '(glossaries)))

	;; Auto symbols completion in status line (E.g. C-c RET \gls RET "Tab to complete")
	(TeX-add-symbols
	 ;; Print the lists in the document
	 '("printglossary" ignore)
	 '("printacronyms" ignore)
	 ;; Generate glossaries with xindy/makeindex
	 '("makeglossaries" ignore)
	 ;; Load glossary entries
	 '("loadglsentries" [ "id" ] [ "filename" ])
	 ;; Define Glossary/Acronym
	 '("newacronym" LaTeX-arg-define-glossaries-acronym [ "Short name" ] "Full name")
	 '("newglossaryentry" LaTeX-arg-define-glossaries-glsentry [ "name" ] "description")
	 '("newdualentry" LaTeX-arg-define-glossaries-glsentry [ "name" ] "description")
	 ;; Reference glossary entry
	 '("Gls" LaTeX-arg-glossaries-glsentry)
	 '("gls" LaTeX-arg-glossaries-glsentry)
	 '("Glspl" LaTeX-arg-glossaries-glsentry)
 	 '("glspl" LaTeX-arg-glossaries-glsentry)
 	 '("glsentrylong" LaTeX-arg-glossaries-glsentry)
 	 '("Glsentrylong" LaTeX-arg-glossaries-glsentry)
 	 '("glsentrylongpl" LaTeX-arg-glossaries-glsentry)
 	 '("Glsentrylongpl" LaTeX-arg-glossaries-glsentry)
 	 '("glsentryshort" LaTeX-arg-glossaries-glsentry)
 	 '("Glsentryshort" LaTeX-arg-glossaries-glsentry)
 	 '("glsentryshortpl" LaTeX-arg-glossaries-glsentry)
 	 '("Glsentryshortpl" LaTeX-arg-glossaries-glsentry)
 	 '("glsentryfull" LaTeX-arg-glossaries-glsentry)
 	 '("Glsentryfull" LaTeX-arg-glossaries-glsentry)
 	 '("glsentryfullpl" LaTeX-arg-glossaries-glsentry)
 	 '("Glsentryfullpl" LaTeX-arg-glossaries-glsentry)
	 ;; Reference acronyms
	 '("acrshort" LaTeX-arg-glossaries-acronym)
	 '("Acrshort" LaTeX-arg-glossaries-acronym)
	 '("acrshortpl" LaTeX-arg-glossaries-acronym)
	 '("Acrshortpl" LaTeX-arg-glossaries-acronym)
	 '("acrlong" LaTeX-arg-glossaries-acronym)
	 '("Acrlong" LaTeX-arg-glossaries-acronym)
	 '("acrlongpl" LaTeX-arg-glossaries-acronym)
	 '("Acrlongpl" LaTeX-arg-glossaries-acronym)
	 '("acrfull" LaTeX-arg-glossaries-acronym)
	 '("acrfullfmt" LaTeX-arg-glossaries-acronym)
	 '("Acrfull" LaTeX-arg-glossaries-acronym)
	 )

	;; Fontification
	(when (and (fboundp 'font-latex-add-keywords)
			   (fboundp 'font-latex-set-syntactic-keywords)
			   (eq TeX-install-font-lock 'font-latex-setup))
	  (font-latex-add-keywords
	   '(("newacronym" "[{")
		 ("newglossaryentry" "[{")
		 ("newdualentry" "[{")
		 ("gls" "[{")
		 ("Gls" "[{")
		 ("glspl" "[{")
		 ("Glspl" "[{")
		 ("glsentrylong"  "[{")
		 ("Glsentrylong"  "[{")
		 ("glsentrylongpl"  "[{")
		 ("Glsentrylongpl"  "[{")
		 ("glsentryshort"  "[{")
		 ("Glsentryshort"  "[{")
		 ("glsentryshortpl"  "[{")
		 ("Glsentryshortpl"  "[{")
		 ("glsentryfull"  "[{")
		 ("Glsentryfull"  "[{")
		 ("glsentryfullpl"  "[{")
		 ("Glsentryfullpl"  "[{")
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
		 ("Acrfull" "[{"))
	   'reference)
	  )
	)))

(defun init-LaTeX-glossaries-style ()
  (TeX-auto-add-regexp TeX-load-glsentries-regexp))

;;; glossaries.el ends here
