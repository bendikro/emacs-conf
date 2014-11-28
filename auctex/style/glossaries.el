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

(TeX-auto-add-type "glossaries-acronym" "LaTeX")
(TeX-auto-add-type "glossaries" "LaTeX")

;;(defvar LaTeX-auto-glossaries nil
;;  "Temporary for parsing \\newmacro definitions.")
;;
;;(defvar LaTeX-auto-glossaries-acronyms nil
;;  "Temporary for parsing \\newmacro definitions.")

(defvar TeX-newacronym-regexp
  '("\\\\newacronym{\\([^}{]*\\)}{\\([^}{]*\\)}{\\([^}{]*\\)}"
    (1 2 3) LaTeX-auto-glossaries-acronym)
  "Matches \newacronym definitions.")

(defvar TeX-newglossary-regexp
  '("\\\\newglossaryentry{\\([^}{]*\\)}"
	(1 1) LaTeX-auto-glossaries)
  "Matches \newglossaryentry definitions.")

(defvar TeX-load-glsentries-regexp
  '("\\\\loadglsentries\\[[^]]*\\]{\\(\\.*[^#}%\\\\\\.\n\r]+\\)}"
	1 TeX-auto-file)
  "Matches \loadglsentries definitions.")

(defun LaTeX-glossaries-cleanup ()
  "Move acronyms from `LaTeX-auto-glossaries' to `LaTeX-glossaries-list'."
;;  (message "LaTeX-glossaries-cleanup LaTeX-auto-glossaries: %s, \nLaTeX-auto-glossaries-acronyms: %s" LaTeX-auto-glossaries LaTeX-auto-glossaries-acronym)
  ;;(message "Adding glsentry: (%s) to LaTeX-glossaries-list. LaTeX-auto-glossaries: %s" LaTeX-auto-glossaries)
  ;;(message "Before - LaTeX-glossaries-list: %s" LaTeX-glossaries-list)
  ;;(message "Before - LaTeX-glossaries-acronym-list: %s" LaTeX-glossaries-acronym-list)
  (mapc (lambda (glsentry)
		  ;;(message "glsentry: %s" glsentry)
		  (add-to-list 'LaTeX-glossaries-list glsentry))
		LaTeX-auto-glossaries)
  ;;(message "After glsentry: %s" LaTeX-glossaries-list)
;;  (mapc (lambda (acronym)
;;		  (add-to-list 'LaTeX-glossaries-list (list acronym)))
;;		LaTeX-auto-glossaries-acronym)
  (mapc (lambda (acronym)
		  (add-to-list 'LaTeX-glossaries-acronym-list acronym))
		LaTeX-auto-glossaries-acronym)
  ;;(message "After - LaTeX-glossaries-list: %s" LaTeX-glossaries-list)
  ;;(message "After - LaTeX-glossaries-acronym-list: %s" LaTeX-glossaries-acronym-list)
)

(defun LaTeX-glossaries-prepare ()
;;  (message "TeX-glossaries-prepare!!!!")
  ;; Clear `LaTeX-auto-glossaries-glsentries' before use.
  (setq LaTeX-auto-glossaries nil)
  (setq LaTeX-auto-glossaries-acronym nil))

(add-hook 'TeX-auto-prepare-hook 'LaTeX-glossaries-prepare)
(add-hook 'TeX-auto-cleanup-hook 'LaTeX-glossaries-cleanup)

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
  (let ((glossary
		 (completing-read (TeX-argument-prompt optional prompt "Glossary")
						  (LaTeX-glossariess-list) nil nil nil
						  'LaTeX-glossaries-history)))
    (if (and definition (not (string-equal "" glossary)))
		(LaTeX-add-glossariess glossary))
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
  (let ((acronym
		 (completing-read (TeX-argument-prompt optional prompt "Acronym")
						  (LaTeX-glossaries-acronym-list) nil nil nil
						  'LaTeX-glossaries-acronym-history)))
    (if (and definition (not (string-equal "" acronym)))
		(LaTeX-add-glossaries-acronyms acronym))
    (TeX-argument-insert acronym optional optional)))

(defun LaTeX-arg-define-glossaries-acronym (optional &optional prompt)
  "Prompt for an acronym completing with known acronyms.
If OPTIONAL is non-nil, insert the resulting value as an optional
argument, otherwise as a mandatory one.  Use PROMPT as the prompt
string."
  (LaTeX-arg-glossaries-acronym optional prompt t))

(defvar LaTeX-glossaries-package-options nil
  "Package options for the glossaries package.")

(TeX-add-style-hook "glossaries"
 (function
  (lambda ()
	(message "glossaries style hook executed")
    (TeX-auto-add-regexp TeX-newacronym-regexp)
    (TeX-auto-add-regexp TeX-newglossary-regexp)
	(TeX-auto-add-regexp TeX-load-glsentries-regexp)

	(LaTeX-add-environments
	 '("glossaries" LaTeX-env-args
	   [TeX-arg-string "Longest glossary"]))

    ;; Completion for the glossary entries in \gls
	(setq TeX-complete-list
		  (append '(("\\gls{\\([^{}\n\r\\%,]*\\)"
					 1 LaTeX-glossariess-list "}")
					("\\Gls{\\([^{}\n\r\\%,]*\\)"
					 1 LaTeX-glossariess-list "}"))
				  TeX-complete-list))

    ;; Completion for the glossary entries in \acr
	(setq TeX-complete-list
		  (append '(("\\acrshort{\\([^{}\n\r\\%,]*\\)"
					 1 LaTeX-glossaries-acronym-list "}")
					("\\acrfull{\\([^{}\n\r\\%,]*\\)"
					 1 LaTeX-glossaries-acronym-list "}"))
				  TeX-complete-list))

    ;; RefTeX support
    (and (fboundp 'reftex-add-index-macros)
		 (reftex-add-index-macros '(glossaries)))

	;; Add symbols for regular completion
	(TeX-add-symbols
	 '("makeglossaries" ignore)
	 ;; Load glossary entries
	 '("loadglsentries" [ "id" ] [ "filename" ])
	 '("printglossary" ignore)
	 '("printacronyms" ignore)
	 ;; Define Acronym
	 '("newacronym" LaTeX-arg-define-glossaries-acronym [ "Short name" ] "Full name")
	 ;; Define gls
	 '("newglossaryentry" LaTeX-arg-define-glossaries-glsentry [ "name" ] "description")

	 ;; Load macros
	 '("Gls" LaTeX-arg-glossaries-glsentry)
	 '("gls" LaTeX-arg-glossaries-glsentry)
	 '("acrshort" LaTeX-arg-glossaries-acronym)
	 '("Acrshort" LaTeX-arg-glossaries-acronym)
	 '("ACRshort" LaTeX-arg-glossaries-acronym)
	 '("acrshortpl" LaTeX-arg-glossaries-acronym)
	 '("Acrshortpl" LaTeX-arg-glossaries-acronym)
	 '("ACRshortpl" LaTeX-arg-glossaries-acronym)
	 '("acrlong" LaTeX-arg-glossaries-acronym)
	 '("Acrlong" LaTeX-arg-glossaries-acronym)
	 '("ACRlong" LaTeX-arg-glossaries-acronym)
	 '("acrlongpl" LaTeX-arg-glossaries-acronym)
	 '("Acrlongpl" LaTeX-arg-glossaries-acronym)
	 '("ACRlongpl" LaTeX-arg-glossaries-acronym)
	 '("acrfull" LaTeX-arg-glossaries-acronym)
	 '("acrfullfmt" LaTeX-arg-glossaries-acronym)
	 '("Acrfull" LaTeX-arg-glossaries-acronym)
	 '("ACRfull" LaTeX-arg-glossaries-acronym)
	 )
	)))

;;; glossaries.el ends here
