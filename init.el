;; Turn off mouse interface early in startup to avoid momentary display
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Add hook for functions that need be be run after packages have been loaded
;; After loading, do (run-hooks 'after-init-load-hook)
(setq package-enable-at-startup nil)

(if (> emacs-major-version 23)
	(package-initialize))

(defvar after-init-load-hook nil
  "Call hooks after loading package")

;; uptimes
(setq emacs-load-start-time (current-time))
(global-linum-mode t)

;; No splash screen please
(setq inhibit-startup-message t)

(setq debug-on-error t)
;; Default to user home dir if not already set
(defvar user-home-dir (getenv "HOME"))
(defvar pymacs-enabled 1)

(setq cask-load-file (concat (file-name-as-directory user-home-dir) ".cask/cask.el"))
; Get the base directory of the emacs config
(setq emacs-config-basedir (file-name-directory user-init-file))
(message "user-init-file: %s" user-init-file)
(message "emacs-config-basedir: %s" emacs-config-basedir)

;; Default to user home dir if not already set
(defvar user-writable-dir emacs-config-basedir)

;; used to store local settings (custom.el) and backups
(setq emacs-config-session-store-dir
	  (expand-file-name (concat (file-name-as-directory user-writable-dir) "session-store/")))

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" emacs-config-basedir))
;; Set path to dependencies
(setq setup-files-dir (expand-file-name "setup" emacs-config-basedir))
(setq emacs-conf-dir (expand-file-name "emacs-conf" emacs-config-basedir))

(setq init-local (expand-file-name "init-local.el" emacs-config-basedir))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path setup-files-dir)
(add-to-list 'load-path emacs-conf-dir)

(setq flymake-run-in-place nil) ; nice default when using tramp

;; Package Manager
;; See ~Cask~ file for its configuration
;; https://github.com/cask/cask
(when (>= emacs-major-version 24)
  (if (require 'cask cask-load-file 'noerror)
	  (cask-initialize )
	(message "cask is not installed!")))

;; Keeps ~Cask~ file in sync with the packages
;; that you install/uninstall via ~M-x list-packages~
;; https://github.com/rdallasgray/pallet
(require 'pallet nil 'noerror)

;; Settings for currently logged in user
;(setq user-settings-dir
;      (concat user-emacs-directory "users/" user-login-name))
;(add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(defun get-ext (file-name)
  (car (cdr (split-string file-name "\\."))))

;; Load all files in the dir
(defun load_libs (dir)
  "Load the files in dir"
  (message "Loading files in directory '%s'" dir)
  ;; Load all files in dir
  (setq path (expand-file-name dir emacs-config-basedir))
  (dolist (file (directory-files path t "\\w+\.elc?$"))
	(when (file-regular-p file)
	  (load file)))
)

(load_libs "local-lisp")

; Path
(when (require 'exec-path-from-shell nil 'noerror)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))


(require 'defaults)
(require 'mode-mappings)

;; Load local init file if it exists
(if (file-exists-p init-local)
	(load-file init-local))

(require 'appearance)
(require 'lang)


(load_libs "setup")
(require 'setup-python)
(require 'setup-ispell)
(if (>= emacs-major-version 24)
	(require 'setup-ggtags)
  (require 'setup-gtags))

(require 'setup-package-menu-mode)

(require 'setup-smooth-scrolling)
(require 'setup-whitespace)
(require 'setup-markdown)
(require 'setup-origami)
(require 'key-bindings)

(setq emacs-user-session-store-dir
	  (expand-file-name (concat (file-name-as-directory emacs-config-session-store-dir) user-login-name)))

(when (string= (getenv "EMACS_NO_BACKUP") nil)
  (setq user-backup-dir (expand-file-name (concat (file-name-as-directory emacs-user-session-store-dir) "backups/")))
  (setq user-autosave-dir (expand-file-name (concat (file-name-as-directory emacs-user-session-store-dir) "autosave/")))

  (message "Using %s for backups" user-backup-dir)
  (message "Using %s for autosave" user-autosave-dir)

  (create-if-no-exists (list user-backup-dir user-autosave-dir))

  ;; Write backup files to users directory
  (setq backup-directory-alist
		`(("." . , user-backup-dir)))

  (setq auto-save-file-name-transforms
		`((".*" ,user-autosave-dir t)))

  (setq auto-save-list-file-prefix user-autosave-dir)

  ;; Make backups of files, even when they're in version control
  (setq vc-make-backup-files t))


;; Conclude init by setting up specifics for the current user
;(when (file-exists-p user-settings-dir)
;  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))

(setq custom-file (expand-file-name "custom.el" emacs-user-session-store-dir))
(message "Using custom-file: %s" custom-file)

(if (file-readable-p custom-file)
	(load custom-file))

(run-hooks 'after-init-load-hook)

(message "Emacs startup time: %f seconds."
         (float-time (time-since emacs-load-start-time)))

;;(startup-with-messages-buffer)
