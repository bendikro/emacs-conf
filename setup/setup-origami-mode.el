(provide 'setup-origami)

(add-hook 'origami-mode-hook
          '(lambda()
             (define-key origami-mode-map (kbd "<S-left>") 'origami-close-node)
             (define-key origami-mode-map (kbd "<S-right>") 'origami-open-node)
             ))
