(provide 'setup-latexmk)

(defcustom resize-compile-window-width 90
       "*The width of the compile window. -1 to disable resizing"
	    :group 'setuptex)

(defun setup-custom-latexmk-cmd()
  "Set custom Latexmk command."
  (add-to-list 'TeX-expand-list (list "%(TeX-build-directory)" '(lambda () TeX-build-directory)))
  (add-to-list 'TeX-expand-list (list "%(buffer-name)" '(lambda () (buffer-name))))
  (add-to-list 'TeX-expand-list (list "%(default-directory)" '(lambda nil default-directory)))
  (add-to-list 'TeX-expand-list (list "%(project-root-dir)" '(lambda nil default-directory)))
  (add-to-list 'TeX-expand-list (list "%(output-file)" '(lambda () (concat TeX-master "." TeX-output-extension))))
  (add-to-list 'TeX-expand-list (list "%(aux-dir)" '(lambda ()  (if (equal "" TeX-build-directory) "" (format "-aux-directory=%s" "build")))))
  (add-to-list 'TeX-expand-list (list "%(out-dir)" '(lambda ()  (if (equal "" TeX-build-directory) "" (format "-output-directory=%s" "build")))))
  (add-to-list 'TeX-expand-list (list "%(-PDF)" '(lambda ()  (if (or TeX-PDF-mode TeX-DVI-via-PDFTeX) "-pdf" ""))))
  (add-to-list 'TeX-expand-list (list "%(default-dir)" '(lambda nil default-directory)))
  (add-to-list 'TeX-expand-list (list "%(pdflatex-args)"
									  '(lambda () (if (or TeX-PDF-mode TeX-DVI-via-PDFTeX)
													  "-pdflatex='pdflatex -synctex=1 -file-line-error -shell-escape'"))))
  (add-to-list 'TeX-expand-list (list "%(misc-args)" '(lambda () "")))
  )

;		  "cd /home/bro/master/master_opp/thesis;"
; "cd %(default-directory); " ;
(defvar get-custom-latexmk-cmd
  (concat "echo PWD: $PWD;"        ; Echo the PWD
		  "latexmk "
		  "%(misc-args) "          ; Extra arguments to latexmk
		  "%(pdflatex-args) "      ; Extra arguments to pdflatex
		  "%(-PDF) "               ; Produce pdf (set TeX-PDF-mode to true)
		  "-bibtex "               ; Do bibtex
		  "%(aux-dir) %(out-dir) " ; Used if TeX-build-directory is not an empty string
		  "%s"                     ; main filename
		  )
  )

;;(add-hook 'LaTeX-mode-hook 'setup-custom-latexmk-cmd)
;;(add-hook 'LaTeX-mode-hook '(lambda ()
;;							  (require 'auctex-latexmk)
;;							  ))

(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 (require 'auctex-latexmk)
			 (auctex-latexmk-setup)
			 (require 'setup-latexmk)
			 (setup-custom-latexmk-cmd)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX configurations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'LaTeX-mode-hook '(lambda ()
							(setq TeX-PDF-mode t)
							(setq TeX-interactive-mode t)
							(setq TeX-source-correlate-mode t)
							(setq TeX-auto-save t)
							(setq TeX-parse-self t)
							(setq reftex-plug-into-AUCTeX t)
							(setq-default TeX-command-default "Latexmk")
							(setq TeX-command-default "Latexmk");
							(setq TeX-build-directory "build/")
							;;(setq-default TeX-master (format "%s/main" TeX-build-directory))
							(setq-default TeX-master "main")
							(setq Latexmk-command get-custom-latexmk-cmd) ; Set the custom latexmk command used by auctex-latexmk.el
))

(defun demolish-tex-compile-buffer()
  (interactive)
  (if (get-buffer-window (TeX-process-buffer-name "main")) ;; Test if the window exists
	  (delete-window (get-buffer-window (get-buffer (TeX-process-buffer-name "main"))))
	))

(add-hook 'LaTeX-mode-hook '(lambda ()
							  (local-set-key (kbd "C-c C-d")
											 'demolish-tex-compile-buffer)
							  (local-set-key (kbd "C-c C-r") 'resize-mode)
							  (local-set-key (kbd "C-c b") 'ispell-word)))

(defun set-window-width(n)
  "Set the selected window's width."
  (window-resize (selected-window) (- n (window-width)) t))

(defun rezize-tex-compile-buffer()
  (interactive)
  (if (/= resize-compile-window-width -1)
	  (if (get-buffer-window (TeX-process-buffer-name "main")) ;; Test if the window exists
		  (with-selected-window (get-buffer-window (TeX-process-buffer-name "main"))
			(set-window-width resize-compile-window-width)
			)
		)))

;; Wrappes around Latexmk-sentinel in auctex-latexmk and resizes the output buffer before the function is called
(defadvice Latexmk-sentinel (around resize-output-window activate)
  (rezize-tex-compile-buffer)
  ad-do-it
)

;:; Sets this as the default pdf view command
(defun setup-qpdfview()
  ;;(setq TeX-view-program-list '(("qpdfview" "qpdfview --instance emacsauxtex --unique \"\"%o\"#src:%(buffer-name):%n:0\"")))
  (setq TeX-view-program-list '(("qpdfview" "qpdfview --instance emacsauxtex --unique \"\"%o\"#src:%(default-dir)%(buffer-name):%n:0\"")))
  (setq TeX-view-program-selection '((output-pdf "qpdfview")))
  )
(add-hook 'LaTeX-mode-hook 'setup-qpdfview)


;(defun demolish-tex-help ()
;  (interactive)
;  (if (get-buffer "*TeX Help*") ;; Tests if the buffer exists
;      (progn ;; Do the following commands in sequence
;        (if (get-buffer-window (get-buffer "*TeX Help*")) ;; Tests if the window exists
;            (delete-window (get-buffer-window (get-buffer "*TeX Help*")))
;          ) ;; That should close the window
;        (kill-buffer "*TeX Help*") ;; This should kill the buffer
;        )
;    )
;  )
;
;(defun run-latexmk ()
;  (interactive)
;  (let ((TeX-save-query nil)
;        (TeX-process-asynchronous nil)
;        (master-file (TeX-master-file)))
;    (TeX-save-document "")
;    (TeX-run-TeX "latexmk"
;				 ;(TeX-command-expand "latexmk -pdflatex='pdflatex -synctex=1 -file-line-error' -pdf %s" 'TeX-master-file)
;				 (TeX-command-expand "latexmk -pdf %s" 'TeX-master-file)
;                 master-file)
;    (if (plist-get TeX-error-report-switches (intern master-file))
;        (TeX-next-error t)
;      (progn
;    (demolish-tex-help)
;    (minibuffer-message "latexmk: done.")))))

;(add-hook 'LaTeX-mode-hook (lambda () (local-set-key (kbd "C-0") 'run-latexmk)))

;(define-key flycheck-map (kbd "C-n") 'flycheck-next-error)

;(eval-after-load "tex" (load "auctex-synctex-evince.el" nil t t))

;;Nomenclature for latex
;(eval-after-load "tex"
;  '(add-to-list 'TeX-command-list
;                '("Nomenclature" "makeindex build/%s.nlo -s nomencl.ist -o build/%s.nls"
;                  (lambda (name command file)
;                    (TeX-run-compile name command file)
;                    (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
;                  nil t :help "Create nomenclature file")))

