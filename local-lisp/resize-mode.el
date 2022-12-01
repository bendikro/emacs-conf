(defvar resize-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-g") 'normal-mode)
    (define-key m (kbd "RET") 'normal-mode)
    (define-key m (kbd "<down>") 'enlarge-window)
    (define-key m (kbd "<up>") 'shrink-window)
    (define-key m (kbd "<left>") 'shrink-window-horizontally)
    (define-key m (kbd "<right>") 'enlarge-window-horizontally)
    m))

(define-minor-mode resize-mode
  "Resize mode"
  :initial-value nil
  :lighter " Resize"
  :keymap resize-mode-map
  :group 'resize)


(defun buffer-mode (&optional buffer-or-name)
  "Returns the major mode associated with a buffer.
If buffer-or-name is nil return current buffer's mode."
  (interactive)
  (setq-default major-mode 'resize-mode)
  )

(provide 'resize)
