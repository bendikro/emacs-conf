;;;;;;;;;;;;;;;;;;;;;
;;;; Python
;;;;;;;;;;;;;;;;;;;;;

;; Set python tab width to 4 && Only spaces
(defun py-indent ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4
        py-indent-offset 4
        indent-tabs-mode nil
        py-smart-indentation nil))
(add-hook 'python-mode-hook 'py-indent)


;;;;;;;;;;;;;;;;;;;;;
;;;; C
;;;;;;;;;;;;;;;;;;;;;

(require 'cc-mode)

;; Fix indent in c-mode
(setq c-default-style "bsd"
      c-basic-offset 4)

;; Set indent level of left curl to 0 instead of default 2
(setq c-offsets-alist
      '((substatement-open . 0)))

(defun my-build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
		(counter 1)
		(ls nil))
	(while (<= counter num-tab-stops)
	  (setq ls (cons (* width counter) ls))
	  (setq counter (1+ counter)))
	(set (make-local-variable 'tab-stop-list) (nreverse ls))))

(defun my-c-mode-common-hook ()
  (setq tab-width 8) ;; change this to taste, this is what K&R uses :)
  (my-build-tab-stop-list tab-width)
  (setq c-basic-offset tab-width))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;;;;;;;;;;;;;;;;;;;;;
;;;; Java
;;;;;;;;;;;;;;;;;;;;;
;; Sets the basic indentation for Java source files
;; to four spaces.
(defun my-java-mode-hook ()
  (setq c-basic-offset 4))
(add-hook 'java-mode-hook 'my-java-mode-hook)

;; If you want Emacs to defer loading the JDE until you open a java file
(setq defer-loading-jde t)

(provide 'setup-lang)