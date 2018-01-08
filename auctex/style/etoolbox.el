;;; xparse.el --- AUCTeX style for `xparse.sty' version 4467.

;; Copyright (C) 2013 Free Software Foundation, Inc.

;; Maintainer: auctex-devel@gnu.org
;; Author: Mosè Giordano <giordano.mose@libero.it>
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

;;; Commentary:

;; This file adds basic support for `xparse.sty' version 4467.  It
;; doesn't parse argument specification of macros and environments.

;;; Code:


(TeX-add-style-hook
 "etoolbox"
 (lambda ()
   (TeX-add-symbols
    ;; Declaring commands and environments

	'("newrobustcmd" TeX-arg-define-macro [ TeX-arg-define-macro-arguments ] [ "optarg default" ] t)
	'("newrobustcmd*" TeX-arg-define-macro [ TeX-arg-define-macro-arguments ] [ "optarg default" ] t)

    '("ifboolexpr" 3)
    '("ifboolexpe" 3)
    '("whileboolexpr" 2)
    '("unlessboolexpr" 2)
    '("DeclareListParser" TeX-arg-macro "Separator")
    '("docsvlist" "Items")
	)
   ;; Fontification
   (when (and (featurep 'font-latex)
	      (eq TeX-install-font-lock 'font-latex-setup))
     (font-latex-add-keywords '(("newrobustcmd" "*{{{")
								("ifboolexpr" "{{{")
								("protected@csxdef" "{{{")
								)
			      'function)))
 LaTeX-dialect)

(defun LaTeX-etoolbox-package-options ()
  "Read the xparse package options from the user."
  (TeX-read-key-val t '(("log-declarations" ("true" "false")))))

;;; xparse.el ends here
