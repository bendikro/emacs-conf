;; Turn off mouse interface early in startup to avoid momentary display
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; uptimes
(setq emacs-load-start-time (current-time))
(global-linum-mode t)

;; No splash screen please
(setq inhibit-startup-message t)

; user-init-file: /home/bro/.emacs.d/init.el

; Get the base directory of the emacs config
;(setq emacs-config-basedir (file-name-directory "/home/bro/.emacs.d/init.el"))
(setq emacs-config-basedir (file-name-directory user-init-file))
(message "emacs-config-basedir: %s" emacs-config-basedir)

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" emacs-config-basedir))

;; Set up load path
(add-to-list 'load-path emacs-config-basedir)
(add-to-list 'load-path site-lisp-dir)

;; Settings for currently logged in user
;(setq user-settings-dir
;      (concat user-emacs-directory "users/" user-login-name))
;(add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Load all files in the dir
(defun load_libs (dir)
  "Load the files in dir"
  (message "Loading files in directory '%s'" dir)
  ;; Load all files in dir
  (setq path (expand-file-name dir emacs-config-basedir))
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

(unless (require 'ps-ccrypt nil 'noerror)
  (message "ps-ccrypt not installed!"))

(eval-after-load 'shell '(require 'setup-shell))

;; Write backup files to users directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Conclude init by setting up specifics for the current user
;(when (file-exists-p user-settings-dir)
;  (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))

(message "Emacs startup time: %d seconds."
         (time-to-seconds (time-since emacs-load-start-time)))
