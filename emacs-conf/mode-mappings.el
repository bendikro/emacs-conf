(provide 'mode-mappings)

;; Emacs lisp
(add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))

;; Set C-mode for Cuda files
(setq auto-mode-alist (cons '("\\.cu$" . c-mode) auto-mode-alist))

;; Python mode for .py files
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))

;; Auctex mode for .tex files
(setq auto-mode-alist (cons '("\\.tex$" . LaTeX-mode) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.tikz\\'" . Latex-mode)) auto-mode-alist))

;; Assembly settings
(require 'asm-mode)
(setq auto-mode-alist (append '(("\\.s$" . asm-mode) ("\\.S$" . asm-mode)) auto-mode-alist))
(setq asm-comment-char ?#)

;; HTML
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.tag$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.vm$" . html-mode))
(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Apache config
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))


