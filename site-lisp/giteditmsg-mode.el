;;;###autoload
(define-derived-mode giteditmsg-mode conf-unix-mode "Git edit message"
  "A major mode for editing .gitignore files."
  (conf-mode-initialize "#")
  (setq fill-column 70))

(provide 'giteditmsg-mode)
