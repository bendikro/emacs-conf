(provide 'mode-mappings)

;; Emacs lisp
(add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))

;; Set C-mode for Cuda files
(setq auto-mode-alist (cons '("\\.cu$" . c-mode) auto-mode-alist))

;; Python mode for .py files
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))

;(defun custom-TeX-command-master (orig-fun &rest args)
;       (message "TeX-command-master called with args %S" args)
;       (let ((res (apply orig-fun args)))
;         (message "TeX-command-master returned %S" res)
;         res))

(defun setup-latex-environment()
  (require 'setup-auctex)
  (require 'setup-latexmk)
  (require 'setup-tex)
  (LaTeX-mode)
;  (advice-add 'TeX-command-master :around #'custom-TeX-command-master)
  )

(setq auto-mode-alist (cons '("\\.tikz$" . LaTeX-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cbx$" . LaTeX-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tex$" . setup-latex-environment) auto-mode-alist))

;; Assembly settings
(setq auto-mode-alist (append '(("\\.s$" . asm-mode) ("\\.S$" . asm-mode)) auto-mode-alist))
(setq asm-comment-char ?#)

;; PHP settings
(require 'php-mode)
(setq auto-mode-alist (append '(("\\.php$" . php-mode)) auto-mode-alist))

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Apache config
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))


