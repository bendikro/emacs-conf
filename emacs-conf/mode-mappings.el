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

(defun enable-guess-style()
  (interactive)
  (require 'guess-style)
  (guess-style-guess-all)
  (guess-style-info-mode 1)
  )

;; LaTeX
(defun setup-latex-environment()
  (interactive)
  (require 'setup-auctex)
  (require 'setup-latexmk)
  (require 'setup-tex)
;  (advice-add 'TeX-command-master :around #'custom-TeX-command-master)
  )

(add-to-autoload "tex-mode" '("\\.tex$" "\\.cbx$" "\\.bbx$" "\\.tikz$" "\\.dtx$") 'setup-latex-environment)

;; Emacs lisp
(add-to-autoload "emacs-lisp-mode" '("\\.Carton$"))

;; Set C-mode for Cuda files
(add-to-autoload "c-mode" '("\\.cu$" "\\.stp$"))

;; Python mode for .py files
;;(add-to-autoload "python-mode" '("\\.py$"))

;; Makefile2 (cmake)
(add-to-list 'auto-mode-alist '("Makefile2" . makefile-mode))

;; Markdown
(add-to-autoload "markdown-mode" '("\\.markdown$" "\\.md$"))

;; Dockerfile
(add-to-autoload "rockerfile-mode" '("Dockerfile"))
(add-to-autoload "rockerfile-mode" '("Rockerfile"))

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

;; conf-mode
(add-to-autoload "conf-mode" '("\\.ini$" ".gitignore$" ".gitconfig$"))

;; jinja2-mode
(add-to-autoload "jinja2-mode" '("\\.j2$"))

(add-to-list 'auto-mode-alist '("\\.bashrc\\'" . sh-mode))

(require 'giteditmsg-mode)
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG\\'" . giteditmsg-mode))


(add-hook 'js-mode-hook
		  '(lambda()
			 (setq-default indent-tabs-mode nil)
			 ))

;(autoload 'gpicker "gpicker" "Gpicker mode" t)
