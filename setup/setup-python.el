(provide 'setup-python)

;; Ignore error when python-mode is unavailable
(ignore-errors
  ;; Disable eldoc automatically opening *Python Help* buffer
  (global-eldoc-mode -1))

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

(when (bound-and-true-p pymacs-enabled) ;; Note that using single-quote causes error
  (message "Pymacs enabled. Disable by setting pymacs-enabled to nil")

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
  ;; These funcions are the function names in site-py/pyfuncs.py
  ;; prefixed with 'pyfuncs-' and undercore replaced with dash
  (global-set-key [f7] 'pyfuncs-break-on-whitespace)
  (global-set-key [f4] 'pyfuncs-convert-attr-to-dict-loop)
  )

;; Enable jedi mode for python
(defun setup-jedi-mode ()
  (interactive)
  (message "Setting up Jedi mode")
  (setq jedi:tooltip-method nil)
  (setq jedi:use-shortcuts t)
  ;;(setq jedi:setup-keys t)
  (safe-wrap
   (jedi:setup)
   (message "Failed to setup Jedi mode for python"))

  ;; Already predefined
  ;;(define-key jedi-mode-map (kbd "M-.") 'jedi:goto-definition)
  (define-key jedi-mode-map (kbd "M-*") 'jedi:goto-definition-pop-marker)
  )

(defun load-jedi-mode-if-in-venv ()
  (let ((venv (getenv "VIRTUAL_ENV")))
	(when venv
		(progn
		  (message "Active virtual environment: %s" venv)
		  (message "Loading jedi-mode")
		  (setup-jedi-mode)
		  )
		)
	)
  )

;; Enable jedi mode for python
(defun on-python-hook ()
  (load-jedi-mode-if-in-venv)
  (require 'yapify-options)
  (define-key python-mode-map (kbd "C-c C-f") 'yapfify-region)
  )

(add-hooks 'on-python-hook '(python-mode-hook))

(defvar jedi:install-pip-packages-command
  `("pip" "install" "jedi" "epc"))

(defun jedi-install-pip-packages ()
  "Install python packages required to run jedi server"
  (interactive)
  (setq resolved-command (jedi:server-pool--resolve-command jedi:install-pip-packages-command))
  (message "Installing pip packages: %s" resolved-command)
  (setq output
		(substring
		 (shell-command-to-string (format "%s" resolved-command))
		 0 -1))
  (message "Command output: %s" output)
  )

(defun dict-pretty-print()
  (interactive)
  (pyfuncs-dict-prettify-region))
