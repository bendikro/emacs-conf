;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; GLOBAL (GTAGS)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-gtags)
(require 'gtags)

; Autoload gtags-mode on c-mode
(add-hook 'c-mode-hook '(lambda () (gtags-mode 1) ))

(add-hook 'gtags-mode-hook
		  '(lambda()
			 (message "gtags-mode-hook")
			 ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			 ;; ;;;;;;; Key bindings for gtag
			 ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

			 (global-set-key "\M-," 'ww-next-gtag)      ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
			 (global-set-key "\M-*" 'gtags-pop-stack)   ;; M-, cycles to next result, after doing M-. C-M-. or C-M-,
			 (global-set-key "\M-." 'gtags-find-tag2)    ;; M-. finds tag
			 ;;(global-set-key [(control meta .)] 'gtags-find-rtag)   ;; C-M-. find all references of tag
			 ;;(global-set-key [(control meta ,)] 'gtags-find-symbol) ;; C-M-, find all usages of symbol.

			 (global-set-key "\M--" 'gtags-find-rtag)    ;; M-- finds tag references

			 ;; Update TAGS file with F9
			 (global-set-key (kbd "<f9>") 'gtags-update-current-file)
			 ))


;(autoload 'gtags-mode "gtags" "" t)

;;;;  Update GTAGS for entire project with "gtags-update-all"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gtags-root-dir()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
      nil)))

(defun gtags-update()
  "Make GTAGS incremental update"
  (interactive)
  (call-process "global" nil nil nil "-u"))

(defun gtags-update-all()
  (when (gtags-root-dir)
    (gtags-update)))

;; Automatically call gtags-update-all on save
;;(add-hook 'after-save-hook #'gtags-update-all)


;;;;  Update current file with "gtags-update"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))

(defun gtags-update-on-save()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))

;; Automatically call gtags-update on save
;;(add-hook 'after-save-hook 'gtags-update-on-save)

(defun ww-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
    (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                            (buffer-list)) ))))
    (cond (latest-gtags-buffer
      (switch-to-buffer latest-gtags-buffer)
      (forward-line)
      (gtags-select-it nil))
    )))

;;;;  To have the last used TAG be the default value (when no current-token is found (when cursor is not on a word))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar gtags-last-token nil
  "Current buffer.")

(defun gtags-find-tag2 (&optional other-win)
  "Input tag name and move to the definition."
  (interactive)
  (let (tagname prompt input)
    (setq tagname (gtags-current-token))
    (if (equal nil tagname)
	(setq tagname gtags-last-token))
    (if tagname
      (setq prompt (concat "Find tag: (default " tagname ") "))
     (setq prompt "Find tag: "))
    (setq input (completing-read prompt 'gtags-completing-gtags
                  nil nil nil gtags-history-list))
    (if (not (equal "" input))
      (setq tagname input))
    (setq gtags-last-token tagname)
    (gtags-push-context)
    (gtags-goto-tag tagname "" other-win)))
