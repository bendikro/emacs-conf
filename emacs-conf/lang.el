(provide 'lang)

;;;;;;;;;;;;;;;;;;;;;
;;;; Python
;;;;;;;;;;;;;;;;;;;;;

;; Set python tab width to 4 && Only spaces
(defun setup-py-indent (ident-width)
  (message "Setting python indent style: %s" ident-width)
  (setq indent-tabs-mode nil)
  (setq tab-width ident-width)
  (setq py-indent-offset ident-width)
  (setq python-indent ident-width)
  (setq py-smart-indentation nil))

(defun python-setup-hook ()
  (require 'python-pep8 nil 'noerror)
  (require 'python-pylint nil 'noerror)
  (require 'python-flake8 nil 'noerror)
  (require 'flycheck nil 'noerror)
  (setup-py-indent 4)
)
(add-hook 'python-mode-hook 'python-setup-hook)

;;;;;;;;;;;;;;;;;;;;;
;;;; C
;;;;;;;;;;;;;;;;;;;;;

(require 'cc-mode)

;; Set indent level of left curl to 0 instead of default 2
(setq c-offsets-alist
      '((substatement-open . 0)))

(defun c-mode-build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
		(counter 1)
		(ls nil))
	(while (<= counter num-tab-stops)
	  (setq ls (cons (* width counter) ls))
	  (setq counter (1+ counter)))
	(set (make-local-variable 'tab-stop-list) (nreverse ls))))

;; Fix indent in c-mode
(defun c-mode-common-setup-hook ()
  (setq tab-width 4) ;; change this to taste, this is what K&R uses :)
  (c-mode-build-tab-stop-list tab-width)
  (setq c-default-style "bsd")
  (setq c-basic-offset tab-width)
)
(add-hook 'c-mode-common-hook 'c-mode-common-setup-hook)

;;;;;;;;;;;;;;;;;;;;;
;;;; C++
;;;;;;;;;;;;;;;;;;;;;

;; function decides whether .h file is C or C++ header, sets C++ by
;; default because there's more chance of there being a .h without a
;; .cc than a .h without a .c (ie. for C++ template files)
(defun c-c++-header ()
  "sets either c-mode or c++-mode, whichever is appropriate for
header"
  (interactive)
  (let ((c-file (concat (substring (buffer-file-name) 0 -1) "c")))
    (if (file-exists-p c-file)
        (c-mode)
      (c++-mode))))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-c++-header))

;;;;;;;;;;;;;;;;;;;;;
;;;; Golang
;;;;;;;;;;;;;;;;;;;;;
(defun go-mode-mode-setup-hook ()
  ;;(global-set-key (kbd "M-.") 'godef-jump)

  (define-key go-mode-map (kbd "M-.") 'xref-find-definitions)
  (define-key go-mode-map (kbd "M-*") 'xref-pop-marker-stack)
  ;;(define-key go-mode-map (kbd "M--") 'xref-find-references)
  (define-key go-mode-map (kbd "M--") 'godef-jump)
  )
(add-hook 'go-mode-hook 'go-mode-mode-setup-hook)


;;;;;;;;;;;;;;;;;;;;;
;;;; Java
;;;;;;;;;;;;;;;;;;;;;
;; Sets the basic indentation for Java source files
;; to four spaces.
(defun java-mode-setup-hook()
  (setq c-basic-offset 4))
(add-hook 'java-mode-hook 'java-mode-setup-hook)

;; If you want Emacs to defer loading the JDE until you open a java file
(setq defer-loading-jde t)

;;;;;;;;;;;;;;;;;;;;;
;;;; Javascript
;;;;;;;;;;;;;;;;;;;;;

(defun js-mode-mode-setup-hook ()
  (setq-default indent-tabs-mode nil)
  (define-key js-mode-map (kbd "C-c C-c") 'comment-line)
  )
(add-hook 'js-mode-hook 'js-mode-mode-setup-hook)


;;;;;;;;;;;;;;;;;;;;;
;;;; Lisp
;;;;;;;;;;;;;;;;;;;;;

(defun lisp-mode-mode-setup-hook ()
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") 'comment-line)
  )
(add-hook 'emacs-lisp-mode-hook 'lisp-mode-mode-setup-hook)


;;;;;;;;;;;;;;;;;;;;;
;;;; Markdown
;;;;;;;;;;;;;;;;;;;;;

(defun markdown-setup-hook ()
  ;; These are bound to reordering lists in markdown mode.
  (define-key markdown-mode-map (kbd "M-<up>") nil)
  (define-key markdown-mode-map (kbd "M-<down>") nil)
)
(add-hook 'markdown-mode-hook 'markdown-setup-hook)

(defun html-mode-mode-setup-hook ()
  (define-key html-mode-map (kbd "C-c C-c") 'comment-line)
  )
(add-hook 'html-mode-hook 'html-mode-mode-setup-hook)


;;;;;;;;;;;;;;;;;;;;;
;;;; dtrt-indent
;;;;;;;;;;;;;;;;;;;;;
(defun load-dtrt-indent-mode ()
  (setq dtrt-indent-verbosity 3)
  (safe-wrap
   (dtrt-indent-mode)
   (message "Failed to load dtrt-indent-mode")))

(add-hooks 'load-dtrt-indent-mode
		   '(python-mode-hook
			 c-mode-hook
			 go-mode-hook

			 makefile-mode-hook
			 sh-mode-hook
			 yaml-mode-hook
			 conf-mode-hook
			 js-mode-hook
			 ))
