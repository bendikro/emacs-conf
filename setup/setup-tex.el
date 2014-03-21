(load "auctex.el" nil t t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activate RefTex and wire it into AucTex ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'turn-on-reftex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX configurations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'TeX-mode-hook '(lambda ()
							(setq TeX-PDF-mode t)
							(setq TeX-interactive-mode t)
							(setq TeX-source-correlate-mode t)
							(setq TeX-auto-save t)
							(setq TeX-parse-self t)
							(setq reftex-plug-into-AUCTeX t)
							(setq-default TeX-command-default "Latexmk")
							(setq TeX-command-default "Latexmk");
							(setq TeX-build-dir "build/")
							(setq-default TeX-master (format "%s/main" TeX-build-dir))
))

(add-hook 'LaTeX-mode-hook 'flyspell-mode nil)

(defun auctex-latexmk-setup2()
  "Add LatexMk command to TeX-command-list."
  (add-hook
   'LaTeX-mode-hook(lambda ()
					 (add-to-list 'TeX-expand-list (list "%(TeX-build-dir)" '(lambda () TeX-build-dir)))
					 (add-to-list 'TeX-expand-list (list "%(PDF)" '(lambda ()  (if (or TeX-PDF-mode TeX-DVI-via-PDFTeX) "-pdf" ""))))
					 ))
  (setq-default TeX-command-list
				(cons
				 '("Latexmk" "cd ..; echo $PWD;  latexmk -pdflatex='pdflatex -synctex=1 -file-line-error' %(PDF) -bibtex -aux-directory=%(TeX-build-dir) -output-directory=%(TeX-build-dir) %s" TeX-run-latexmk nil
				   (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMk")
				 TeX-command-list)
				LaTeX-clean-intermediate-suffixes
				(append LaTeX-clean-intermediate-suffixes
						'("\\.fdb_latexmk" "\\.aux.bak")))
  )


;;Nomenclature for latex
;(eval-after-load "tex"
;  '(add-to-list 'TeX-command-list
;                '("Nomenclature" "makeindex build/%s.nlo -s nomencl.ist -o build/%s.nls"
;                  (lambda (name command file)
;                    (TeX-run-compile name command file)
;                    (TeX-process-set-variable file 'TeX-command-next TeX-command-default))
;                  nil t :help "Create nomenclature file")))


(require 'flymake)

(require 'auctex-latexmk)
(auctex-latexmk-setup2)


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
;
;(add-hook 'LaTeX-mode-hook
;          (lambda () (local-set-key (kbd "C-0") #'run-latexmk)))

(eval-after-load "tex"
  (load "auctex-synctex-evince.el" nil t t)
)
