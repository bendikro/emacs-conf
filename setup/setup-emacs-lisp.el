(eval-when (compile load eval)
  (defvar extra-elisp-primitive-keywords
	'(
	  "getenv"
	  "load"
	  "list"
	  "message"
	  "run-hooks"
	  "setq"
	  )
	"Extra elisp keywords."))

(defvar extra-elisp-font-lock-keywords
  (eval-when-compile
	`(
	  (,(regexp-opt extra-elisp-primitive-keywords 'symbols) . font-lock-keyword-face)
	  ))
  "Extra keyword highlighting specification for `emacs-lisp-mode'.")

(add-hook 'emacs-lisp-mode-hook
		  #'(lambda()
			 (font-lock-add-keywords 'emacs-lisp-mode extra-elisp-font-lock-keywords)
			 ))

;; auto compile elisp files after save
;(add-hook 'emacs-lisp-mode-hook (lambda () (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)) )

(provide 'setup-emacs-lisp)
