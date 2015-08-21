(provide 'setup-ggtags)
(require 'ggtags)

(add-hook 'ggtags-mode-hook
		  '(lambda()
			 (message "gttags-mode-hook")
			 ;; ggtags
			 ;;(global-set-key "\M-." 'ggtags-find-tag2)    ;; M-. finds tag
			 ;;(global-set-key "\M-," 'ggtags-find-tag-continue)    ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
			 ;(define-key ggtags-mode-map (kbd "M-.") 'ggtags-find-tag)
			 ;;(define-key ggtags-mode-map (kbd "M-.") 'ggtags-find-tag-dwim2)
			 ;;(define-key ggtags-mode-map (kbd "M-.") 'ggtags-find-reference)
			 (define-key ggtags-mode-map (kbd "M-,") 'ggtags-find-tag-continue)
			 ;;(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
			 (define-key ggtags-mode-map (kbd "M--") 'ggtags-find-reference)
			 ;;(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
			 ;;(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
			 ;;(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
			 ;;(global-set-key "\M-*" 'gtags-pop-stack)
			 ;;(global-set-key "\M--" 'gtags-find-rtag)    ;; M-- finds tag references
										;ggtags-next-mark
			 ))
