(defvar flycheck-now-mode-hook nil)

(defvar flycheck-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-g") 'normal-mode)
    (define-key m (kbd "C-n") 'flycheck-next-error)
    (define-key m (kbd "C-p") 'flycheck-previous-error)
    m))

(define-minor-mode flycheck-now-mode
  "Flycheck mode"
  :initial-value nil
  :lighter " Flycheck"
  :keymap flycheck-mode-map
  :group 'flycheck-now
  (flycheck-mode)
  )

(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (interactive)
  (setq-default major-mode 'flycheck-now-mode)
  )

(provide 'flycheck-now)
