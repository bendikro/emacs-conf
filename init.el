;; Turn off mouse interface early in startup to avoid momentary display
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Settings for currently logged in user
;(setq user-settings-dir
;      (concat user-emacs-directory "users/" user-login-name))
;(add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Functions (load all files in defuns-dir)
;(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
;(dolist (file (directory-files defuns-dir t "\\w+"))
;  (when (file-regular-p file)
;    (load file)))

(defun load_libs (dir)
  "Load the files in dir"
  (message "hello")
  ;; Load all files in dir
  (setq path (expand-file-name dir user-emacs-directory))
  (dolist (file (directory-files path t "\\w+"))
	(when (file-regular-p file)
	  (load file)))
)

(load_libs "setup")
(load_libs "defuns")

(require 'defaults)
(require 'mode-mappings)
(require 'key-bindings)
(require 'appearance)
(require 'lang)
(require 'setup-gtags)

(require 'setup-smooth-scrolling)
(require 'show-wspace)
(require 'gpicker)

(eval-after-load 'shell '(require 'setup-shell))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Conclude init by setting up specifics for the current user
;(when (file-exists-p user-settings-dir)
;  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
