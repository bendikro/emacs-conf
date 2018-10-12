(provide 'setup-python)

;; Add site-py dir to PYTHONPATH
(setenv "PYTHONPATH"
		(concat
		 (expand-file-name "site-py" emacs-config-basedir) ":" (getenv "PYTHONPATH")
		 ))

;(setq py-load-pymacs-p nil)
;(require 'python-mode nil 'noerror)
;(require 'tramp) ; Slow on startup with no internet connection
(require 'python-pep8 nil 'noerror)
(require 'python-pylint nil 'noerror)
(require 'python-flake8 nil 'noerror)
(require 'flycheck nil 'noerror)

;; python
;(add-font-lock-numbers 'python-mode)


;(defmacro after (mode &rest body)
;  `(eval-after-load ,mode
;     '(progn ,@body)))
;
;(after 'auto-complete-config
;       (ac-config-default)
;       (when (file-exists-p (expand-file-name "~/.emacs.d/elisp/Pymacs"))
;         (ac-ropemacs-initialize)
;         (ac-ropemacs-setup)))


;(defun python-config-python ()
;  "Configure python.el to defaults of using python."
;  (interactive)
;  (setq python-shell-virtualenv-path "venv"
;        python-shell-interpreter "python"
;        python-shell-prompt-regexp ">>> "
;        python-shell-prompt-output-regexp ""
;        ;; python-shell-setup-codes '(python-shell-completion-setup-code python-ffap-setup-code python-eldoc-setup-code)
;        python-shell-completion-module-string-code ""
;        python-shell-completion-string-code "';'.join(__COMPLETER_all_completions('''%s'''))
;"))
;
;(after 'python (python-config-ipython))

;(defmacro after (mode &rest body)
;  `(eval-after-load ,mode
;     '(progn ,@body)))
;
;(after 'auto-complete-config
;       (ac-config-default)
;       (when (file-exists-p (expand-file-name "/Users/dcurtis/.emacs.d/elisp/Pymacs"))
;         (ac-ropemacs-initialize)
;         (ac-ropemacs-setup)))


(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

(setq site-py-dir (expand-file-name "site-py" emacs-config-basedir))
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path site-py-dir))

(pymacs-load "pyfuncs")
(global-set-key [f7] 'pyfuncs-break-on-whitespace)


;; Enable jedi mode for python
(defun setup-jedi-mode ()
  (setq jedi:use-shortcuts t)
  (setq jedi:setup-keys t)
  (safe-wrap
   (jedi:setup)
   (message "Failed to setup Jedi mode for python")))

(add-hooks 'setup-jedi-mode '(python-mode-hook))
