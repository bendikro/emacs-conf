;;; auto-complete-auctex.el --- auto-completion for auctex

;; Copyright (C) 2012 Christopher Monsanto

;; Author: Christopher Monsanto <chris@monsan.to>
;; Version: 1.0
;; Package-Requires: ((yasnippet "0.6.1") (auto-complete "1.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; You can install this by (require 'auto-complete-auctex).
;; Feel free to contribute better documentation!

;;; Code:

(provide 'auto-complete-auctex)

(require 'tex)
(require 'latex)

(eval-when-compile
  (require 'auto-complete)
  (require 'yasnippet))

(defvar ac-auctex-arg-lookup-table
  '((TeX-arg-define-macro . ("\\MacroName"))
    (TeX-arg-counter . ("Counter"))
    (TeX-arg-define-counter . ("\\CounterName"))
    (TeX-arg-file . ("Filename"))
    (TeX-arg-bibliography . ("Filename"))
    (TeX-arg-bibstyle . ("Style"))
    (TeX-arg-environment . ("Environment"))
    (TeX-arg-define-environment . ("EnvironmentName"))
    (TeX-arg-size . ("(w, h)"))
    (TeX-arg-ref . ("Name"))
    (TeX-arg-glossaries . (""))
    (TeX-arg-index . ("Index"))
    (TeX-arg-define-label . ("Label"))
    (LaTeX-arg-usepackage . (["opt1,..."] "Package"))
    (LaTeX-env-label . nil)
    (LaTeX-amsmath-env-aligned . (["htbp!"]))
    (LaTeX-amsmath-env-alignat . (["# Columns"]))
    (LaTeX-env-array . (["bct"] "lcrpmb|"))
    (LaTeX-env-item . nil)
    (LaTeX-env-document . nil)
    (LaTeX-env-figure . (["htbp!"]))
    (LaTeX-env-contents . ("Filename"))
    (LaTeX-env-minipage . (["htbp!"] "Width"))
    (LaTeX-env-list . ("Label" "\\itemsep,\\labelsep,..."))
    (LaTeX-env-picture . ("(w, h)" "(x, y)"))
    (LaTeX-env-tabular* . ("Width" ["htbp!"] "lcrpmb|><"))
    (LaTeX-env-bib . ("WidestLabel"))
    (TeX-arg-conditional . ([""]))
    (2 . ("" ""))
    (3 . ("" "" ""))
    (4 . ("" "" "" ""))
    (5 . ("" "" "" "" ""))
    (6 . ("" "" "" "" "" ""))
    (7 . ("" "" "" "" "" "" ""))
    (8 . ("" "" "" "" "" "" "" ""))
    (9 . ("" "" "" "" "" "" "" "" "")))
  "Anything not in this table defaults to '(\"\")")

(defun ac-auctex-expand-arg-info (arg-info)
  (loop for item in arg-info
	append (cond
		((or (stringp item) (and (vectorp item) (stringp (elt item 0))))
		 (list item))
		((vectorp item)
		 (loop for item-2 in (or (assoc-default (or (car-safe (elt item 0)) (elt item 0))
							ac-auctex-arg-lookup-table 'equal) '(""))
		       collect [item-2]))
		(t
		 (or (assoc-default (or (car-safe item) item) ac-auctex-arg-lookup-table) '(""))))))

(defun ac-auctex-snippet-arg (n arg)
  (ac-auctex-snippet-arg2 n arg))

(defun ac-auctex-snippet-arg2 (n arg)
  (let* ((opt (vectorp arg))
	 (item (if opt (elt arg 0) arg))
	 (m (if (vectorp arg) (1+ n) n))
	 (var (format "${%s}" item)))
    (list (1+ m)
	  (if opt
	      (concat (format "${[") var "]}")
	    (concat "{" var "}")))))

;; Macros
;;

(defun ac-auctex-expand-args (str env)
  (message "ac-auctex-expand-args")
  (yas-expand-snippet (ac-auctex-macro-snippet (assoc-default str env))))

(defun ac-auctex-macro-snippet (arg-info)
  (message "ac-auctex-macro-snippet: %s" arg-info)
  (let ((count 1))
    (apply 'concat
		   (loop for item in (ac-auctex-expand-arg-info arg-info)
				 collect (destructuring-bind (n val)
							 (ac-auctex-snippet-arg count item)
						   (setq count n)
						   val))))
  )

(defun ac-auctex-macro-candidates2 ()
;  (message "ac-auctex-macro-candidates2: %s" (ac-auctex-macro-candidates))
;  (message "TeX-symbol-list: %s" TeX-symbol-list)
  (ac-auctex-macro-candidates))

(defun ac-auctex-macro-candidates ()
  (message "ac-auctex-macro-candidates")
   (let ((comlist (if TeX-symbol-list
					  (mapcar (lambda (item)
								(or (car-safe (car item)) (car item)))
							  TeX-symbol-list))))
	 (all-completions ac-prefix comlist)))


(defun ac-auctex-macro-action2 ()
  (ac-auctex-macro-action)
  (message "ac-auctex-macro-action2"))

(defun ac-auctex-macro-action ()
;  (message "ac-auctex-macro-action")
  (yas-expand-snippet (ac-auctex-macro-snippet (assoc-default candidate TeX-symbol-list))))

(ac-define-source auctex-macros
  '((init . TeX-symbol-list)
    (candidates . ac-auctex-macro-candidates2)
    (action . ac-auctex-macro-action2)
    (requires . 0)
    (symbol . "m")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))

;; Symbols
;;

(defun ac-auctex-symbol-candidates ()
  (message "ac-auctex-symbol-candidates")
  (all-completions ac-prefix (mapcar 'cadr LaTeX-math-default)))

(defun ac-auctex-symbol-action ()
  (message "ac-auctex-symbol-action")
  (re-search-backward candidate)
  (delete-region (1- (match-beginning 0)) (match-end 0))
  (if (texmathp)
      (progn
	(insert "\\" candidate)
	(yas-expand-snippet (ac-auctex-macro-snippet (assoc-default candidate TeX-symbol-list))))
    (insert "$\\" candidate "$")
    (backward-char)
    (yas-expand-snippet (ac-auctex-macro-snippet (assoc-default candidate TeX-symbol-list)))))

(defun ac-auctex-symbol-document (c)
  (let* ((cl (assoc c (mapcar 'cdr LaTeX-math-default)))
         (decode (if (nth 2 cl) (char-to-string (decode-char 'ucs (nth 2 cl))) ""))
         (st (nth 1 cl))
         (hs (if (listp st) (mapconcat 'identity st " ") st)))
    (and decode (concat hs " == " decode))))

(ac-define-source auctex-symbols
  '((init . LaTeX-math-mode)
    (candidates . ac-auctex-symbol-candidates)
    (document . ac-auctex-symbol-document)
    (action . ac-auctex-symbol-action)
    (requires . 0)
    (symbol . "s")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))


;; Environments
;;


(defvar ac-auctex-environment-prefix "beg")

(defun ac-auctex-environment-candidates ()
  (let ((envlist (mapcar (lambda (item) (concat ac-auctex-environment-prefix (car item)))
			 LaTeX-environment-list)))
    (all-completions ac-prefix envlist)))

(defun ac-auctex-environment-action ()
  (re-search-backward candidate)
  (delete-region (1- (match-beginning 0)) (match-end 0))
  (let ((candidate (substring candidate (length ac-auctex-environment-prefix))))
    (yas-expand-snippet (format "\\begin{%s}%s\n$0\n\\end{%s}"
				candidate
				(ac-auctex-macro-snippet (assoc-default candidate LaTeX-environment-list))
				candidate))))

(ac-define-source auctex-environments
  '((init . LaTeX-environment-list)
    (candidates . ac-auctex-environment-candidates)
    (action .  ac-auctex-environment-action)
    (requires . 0)
    (symbol . "e")
    (prefix . "\\\\\\([a-zA-Z]*\\)\\=")))

;; Refs
;;


(defun ac-auctex-label-candidates ()
  ;;(message "ac-auctex-label-candidates: %s" (all-completions ac-prefix (mapcar 'car LaTeX-label-list)))
  (all-completions ac-prefix (mapcar 'car LaTeX-label-list)))

(ac-define-source auctex-labels
  '((init . LaTeX-label-list)
    (candidates . ac-auctex-label-candidates)
    (requires . 0)
    (symbol . "r")
    (prefix . "\\\\\\(?:c\\)?ref{\\([^}]*\\)\\=")))


;; Bibs
;;

(defun ac-auctex-bib-candidates ()
  (all-completions ac-prefix (mapcar 'car LaTeX-bibitem-list)))

(ac-define-source auctex-bibs
  `((init . LaTeX-bibitem-list)
    (candidates . ac-auctex-bib-candidates)
    (requires . 0)
    (symbol . "b")
    (prefix . ,(concat "\\\\cite\\(?:n\\|np\\)?"
					   "\\(?:"
					   "\\[[^]]*\\]"
					   "\\)?"
					   "{\\([^},]*\\)"
					   "\\="))))

;; Glossaries
;;

(defvar auctex-completion-list-tmp (list)
  "Temporary for parsing \\newmacro definitions.")

(defun add-list-to-list (elements add-to-list)
  ;;(message "add-list-to-list: %s" elements)
  (mapc (lambda (listelem)
		  ;;(message "glsentry: %s" glsentry)
		  (setq tmp (nth 0 listelem))
		  (if (listp tmp)
			  (progn
				;;(message "is a list: %s" tmp)
				(setq add-to-list (add-list-to-list listelem add-to-list))
				)
			(progn
			  ;;(message "NOt a list: %s so adding %s" tmp listelem)
			  (add-to-list 'add-to-list listelem)
			  ;;(message "add-to-listis now: %s" add-to-list)
			)
			))
		  ;;(add-to-list 'add-to-list listelem))
		elements)
  ;;(message "add-to-list ret: %s" add-to-list)
  add-to-list
)

(defun ac-auctex-glossaries-candidates ()
;;  (message "ac-auctex-glossaries-candidates - ac-prefix: %s, LaTeX-glossaries-list: %s" ac-prefix LaTeX-glossaries-list)
  (all-completions ac-prefix (mapcar 'car auctex-completion-list-tmp)))

(defun ac-auctex-glossaries-init ()
  (interactive)
  (setq auctex-completion-list-tmp (list nil))
  (setq auctex-completion-list-tmp
		(add-list-to-list LaTeX-glossaries-list auctex-completion-list-tmp))
  (setq auctex-completion-list-tmp
		(add-list-to-list LaTeX-glossaries-acronym-list auctex-completion-list-tmp))
  auctex-completion-list-tmp)

(defun ac-auctex-glossaries-acronyms-init ()
  (interactive)
  (setq auctex-completion-list-tmp (list nil))
  (setq auctex-completion-list-tmp
		(add-list-to-list LaTeX-glossaries-acronym-list auctex-completion-list-tmp))
  auctex-completion-list-tmp)

(ac-define-source auctex-glossaries
  `((init . ac-auctex-glossaries-init)
    (candidates . ac-auctex-glossaries-candidates)
    (requires . 0)
    (symbol . "g")
    (prefix . ,(concat "\\(?:gls\\|Gls\\)"
					   "\\(?:pl\\)?"
					   "\\(?:"
					   "\\[[^]]*\\]"
					   "\\)?"
					   "{\\([^},]*\\)"
					   "\\="))))
;; Acronyms
;;
(ac-define-source auctex-glossaries-acronym
  `((init . ac-auctex-glossaries-acronyms-init)
    (candidates . ac-auctex-glossaries-candidates)
    (requires . 0)
    (symbol . "a")
    (prefix . ,(concat "\\(?:acrshort\\|Acrshort\\)"
					   "\\(?:pl\\)?"
					   "\\(?:"
					   "\\[[^]]*\\]"
					   "\\)?"
					   "{\\([^},]*\\)"
					   "\\="))))

;; Setup
;;
(defun ac-auctex-setup ()
  (message "ac-auctex-setup")
  (setq ac-sources (append
                      '(ac-source-auctex-symbols
                        ac-source-auctex-macros
						ac-source-auctex-environments
						ac-source-auctex-labels
						ac-source-auctex-bibs
						ac-source-auctex-glossaries
						ac-source-auctex-glossaries-acronym
						)
                      ac-sources)))

(add-to-list 'ac-modes 'latex-mode)

(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 (ac-auctex-setup)
			 (setq ac-use-menu-map t)
			 ;; Default settings [1;5B
;			 (define-key ac-menu-map "\e[3;3~" [down])
			 (define-key ac-menu-map "\C-n" 'ac-next)
			 (define-key ac-menu-map "\C-p" 'ac-previous)
			 ;(define-key ac-menu-map "\C-m" 'ac-next)
			 (define-key ac-menu-map "\eOB" 'ac-next)
			 (define-key ac-menu-map "\eOA" 'ac-previous)
			 ;(define-key ac-menu-map [up] 'ac-previous)
			 ;(define-key ac-menu-map [down] 'ac-next)
			 ;(define-key ac-menu-map (kbd "<up>") 'ac-previous)
			 ;(define-key ac-menu-map (kbd "<down>") 'ac-next)
			 ;(global-set-key (kbd "<up>") 'ac-previous)
			 ;(global-set-key (kbd "<down>") 'ac-next)
			 ))

;;; auto-complete-auctex.el ends here
