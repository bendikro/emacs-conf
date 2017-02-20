(require 'dockerfile-mode)

(defgroup rockerfile nil
  "rockerfile code editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "rockerfile-"
  :group 'languages)

(defcustom rockerfile-mode-hook nil
  "*Hook called by `rockerfile-mode'."
  :type 'hook
  :group 'rockerfile)

(defvar rockerfile-font-lock-keywords
  `(,(cons (rx (or line-start "onbuild ")
               (group (or "from" "maintainer" "run" "cmd" "expose" "env" "arg"
                          "add" "copy" "entrypoint" "volume" "user" "workdir" "onbuild"
                          "label" "mount" "tag" "export" "import" "push" "attach"))
               word-boundary)
           font-lock-keyword-face)
    ,@(sh-font-lock-keywords)
    ,@(sh-font-lock-keywords-2)
    ,@(sh-font-lock-keywords-1))
  "Default font-lock-keywords for `rockerfile mode'.")

;; Handle emacs < 24, which does not have prog-mode
(defalias 'rockerfile-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode rockerfile-mode rockerfile-parent-mode "Rockerfile"
  "A major mode to edit Rockerfiles.
\\{rockerfile-mode-map}
"
  (set-syntax-table rockerfile-mode-syntax-table)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'font-lock-defaults)
       '(rockerfile-font-lock-keywords nil t))
  (setq local-abbrev-table rockerfile-mode-abbrev-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("Rockerfile.*\\'" . rockerfile-mode))

(provide 'rockerfile-mode)
