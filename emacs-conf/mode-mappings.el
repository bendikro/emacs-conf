(provide 'mode-mappings)

(defmacro create-safe-loader-func ()
  (let ((funsymbol (intern (concat "safe-load-mode-" mode))))
	`(defun ,funsymbol () (interactive)
	   (if (not (eq ',func nil))
		   (,func))
	   (message "Loading mode: %s" ,mode)
	   (if (condition-case nil (require (intern ,mode)) (error nil))
		 (,(intern mode)); Enabling mode if available
		 (message "'%s' not available" ,mode))
	   )))

(defun add-to-autoload (mode extensions &optional func)
  (create-safe-loader-func)
  (dolist (ext extensions)
	(add-to-list 'auto-mode-alist (cons ext (intern (concat "safe-load-mode-" mode))))))

;(defun custom-TeX-command-master (orig-fun &rest args)
;       (message "TeX-command-master called with args %S" args)
;       (let ((res (apply orig-fun args)))
;         (message "TeX-command-master returned %S" res)
;         res))

;; LaTeX
(defun setup-latex-environment()
  (interactive)
  (load "auctex.el" nil t t)
  (require 'setup-auctex)
  (require 'setup-latexmk)
  (require 'setup-tex)
;  (advice-add 'TeX-command-master :around #'custom-TeX-command-master)
  )

(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 (setup-latex-environment)
			 ))

;(add-to-autoload "LaTeX-mode" '("\\.tex$" "\\.sty$" "\\.cbx$" "\\.bbx$" "\\.tikz$" "\\.dtx$") 'setup-latex-environment)
;(add-to-autoload "latex-mode" '("\\.tex$" "\\.sty$" "\\.cbx$" "\\.bbx$" "\\.tikz$" "\\.dtx$") 'setup-latex-environment)

; (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

;; Emacs lisp
(add-to-autoload "emacs-lisp-mode" '("\\.Carton$"))

;; Set C-mode for Cuda files
(add-to-autoload "c-mode" '("\\.cu$" "\\.stp$"))

;; Python mode for .py files
;;(add-to-autoload "python-mode" '("\\.py$"))

;; Markdown
(add-to-autoload "markdown-mode" '("\\.markdown$" "\\.md$"))

;; Assembly settings
(add-to-autoload "asm-mode" '("\\.s$" "\\.S$" ))
(setq asm-comment-char ?#)

;; PHP settings
(add-to-autoload "php-mode" '("\\.php$"))

;; HTML
(add-to-autoload "html-mode" '("\\.html$" "\\.tag$" "\\.vm$"))

;; org-mode
(add-to-autoload "org-mode" '("\\.org$"))

;; go-mode
(add-to-autoload "go-mode" '("\\.go$"))

;; yaml-mode
(add-to-autoload "yaml-mode" '("\\.yml$" "\\.yaml$"))

;; Apache config
(add-to-autoload "apache-mode" '("\\.htaccess\\'" "httpd\\.conf\\'" "srm\\.conf\\'" "access\\.conf\\'" "sites-\\(available\\|enabled\\)/"))

;; ps-ccrypt
(unless (require 'ps-ccrypt nil 'noerror)
  (message "ps-ccrypt not installed!"))

;; rtf-mode
(add-to-autoload "rtf-mode" '("\\.rtf$"))


(require 'giteditmsg-mode)
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . giteditmsg-mode))


(add-hook 'js-mode-hook
		  '(lambda()
			 (setq-default indent-tabs-mode nil)
			 ))

;(autoload 'gpicker "gpicker" "Gpicker mode" t)
