(provide 'setup-tex)

;(add-hook 'LaTeX-mode-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook '(lambda()
							 (setq TeX-error-overview-open-after-TeX-run t)))

;; Auto-complete
(add-hook 'auto-complete-mode-hook
		  '(lambda()
			 (define-key ac-completing-map "\e" 'ac-stop)
			 (yas-minor-mode)
			 (yas-minor-mode-on)
			 (ac-set-trigger-key "TAB")
			 (setq ac-auto-start nil)))

(add-hook 'LaTeX-mode-hook
		  '(lambda()
			 ;;(require 'auto-complete-config)
			 ;;(require 'auto-complete)
			 ;; Compilation output
			 (message "run-hooks: %s" after-init-load-hook)
			 (global-set-key (kbd "C-c C-g") #'encapsulate-glossary)
			 (run-hooks 'after-init-load-hook)
			 (setq compilation-scroll-output t)
			 (auto-complete-mode)))
