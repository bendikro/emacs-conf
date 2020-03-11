(provide 'mode-mappings)

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
  ;; Set the new indent based on python-indent
  (setup-py-indent python-indent)
  )

;; LaTeX
(defun setup-latex-environment()
  (interactive)
  (require 'setup-auctex)
  (require 'setup-latexmk)
  (require 'setup-tex)
;  (advice-add 'TeX-command-master :around #'custom-TeX-command-master)
  )

;; Emacs lisp
(use-package emacs-lisp-mode
  :mode "\\.Carton$")

;;(add-hook 'rockerfile-mode-hook 'whitespace-mode 1)


;; Set C-mode for Cuda files
(use-package c-mode
  :mode ("\\.cu$" "\\.stp$"))

;; Makefile2 (cmake)
(use-package makefile-mode
  :mode "Makefile2")

;; Assembly settings
(use-package asm-mode
  :mode ("\\.s$" "\\.S$")
  :config (setq asm-comment-char ?#))

;; PHP settings
(use-package php-mode
  :mode "\\.php$")

;; HTML
(use-package sgml-mode
  :mode (("\\.html\\'" . html-mode)
         ("\\.tag\\'" . html-mode)
		 ("\\.vm\\'" . html-mode)))

;; go-mode
(use-package go-mode
  :mode "\\.go$")

;; Dockerfile (site-lisp)
(use-package rockerfile-mode
  :mode (("Dockerfile" . rockerfile-mode)
         ("Rockerfile" . rockerfile-mode)))

;; jinja2-mode
(use-package jinja2-mode
  :mode "\\.j2$")

(use-package tex-mode
  :mode ("\\.tex$" "\\.cbx$" "\\.bbx$" "\\.tikz$" "\\.dtx$")
  :config (setup-latex-environment))

;; Markdown
(use-package markdown-mode
  :mode ("\\.markdown\\'" "\\.md\\'"))

;; yaml-mode
(use-package yaml-mode
  :mode ("\\.yml$" "\\.yaml$"))

;; org-mode
(use-package org-mode
  :mode "\\.org$")

;; Apache config
(use-package apache-mode
  :mode ("\\.htaccess\\'" "httpd\\.conf\\'" "srm\\.conf\\'"
		 "access\\.conf\\'" "sites-\\(available\\|enabled\\)/"))

;; rtf-mode (site-lisp)
(use-package rtf-mode
  :mode "\\.rtf$")

;; conf-mode
(use-package conf-mode
  :mode ("\\.ini$" ".gitignore$" ".gitconfig$"))

(use-package sh-mode
  :mode "\\.bashrc\\'")

;; (site-lisp/giteditmsg-mode.el)
(use-package giteditmsg-mode
  :mode "COMMIT_EDITMSG\\'")

;; systemd config files
(use-package conf-unix-mode
  :mode ("\\.service\\'" "\\.timer\\'" "\\.target\\'" "\\.mount\\'"
		 "\\.automount\\'" "\\.slice\\'" "\\.socket\\'" "\\.path\\'"
		 "\\.netdev\\'" "\\.network\\'" "\\.link\\'" "\\.automount\\'"))

;; ps-ccrypt
(unless (require 'ps-ccrypt nil 'noerror)
  (message "ps-ccrypt not installed!"))
