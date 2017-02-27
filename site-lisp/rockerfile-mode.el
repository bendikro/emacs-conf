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
  `(,(cons (rx
			;;(or line-start "onbuild ")
			(or (and line-start (zero-or-more space))  "onbuild ")
               (group (or "from" "maintainer" "run" "cmd" "expose" "env" "arg"
                          "add" "copy" "entrypoint" "volume" "user" "workdir" "onbuild"
                          "label" "mount" "tag" "export" "import" "push" "attach"))
               word-boundary)
           font-lock-keyword-face)
    ,@(sh-font-lock-keywords)
    ,@(sh-font-lock-keywords-2)
    ,@(sh-font-lock-keywords-1))
  "Default font-lock-keywords for `rockerfile mode'.")

(defvar rockerfile-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'dockerfile-build-buffer)
    (define-key map "\C-c\M-b" 'dockerfile-build-no-cache-buffer)
    (define-key map "\C-c\C-z" 'dockerfile-test-function)
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map [menu-bar dockerfile-mode] (cons "Dockerfile" menu-map))
    (define-key menu-map [dfc]
      '(menu-item "Comment Region" comment-region
                  :help "Comment Region"))
    (define-key menu-map [dfb]
      '(menu-item "Build" dockerfile-build-buffer
                  :help "Send the Dockerfile to docker build"))
    (define-key menu-map [dfb]
      '(menu-item "Build without cache" dockerfile-build-no-cache-buffer
                  :help "Send the Dockerfile to docker build without cache"))
    map))

;; Handle emacs < 24, which does not have prog-mode
(defalias 'rockerfile-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode rockerfile-mode rockerfile-parent-mode "Rockerfile"
  "A major mode to edit Rockerfiles.
\\{rockerfile-mode-map}
"
  (set-syntax-table dockerfile-mode-syntax-table)
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
