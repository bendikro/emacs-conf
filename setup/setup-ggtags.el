(provide 'setup-ggtags)

(when (require 'ggtags nil 'noerror)
  ;; Autoload ggtags-mode on c++-mode-hook
  (add-hook 'c++-mode-hook #'(lambda () (ggtags-mode 1) ))
  )

(add-hook 'ggtags-mode-hook
		  #'(lambda()
			 ;; ggtags
			 (define-key ggtags-mode-map (kbd "M-,") 'ggtags-find-tag-continue)
			 ;;(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
			 (define-key ggtags-mode-map (kbd "M--") 'ggtags-find-reference)
			 ;; Do not underline valid tags, e.g. functions
			 (set-face-attribute 'ggtags-highlight nil :underline nil)
			 ;;(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
			 ;;(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
			 ;;(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
			 ))
