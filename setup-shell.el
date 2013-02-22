;; Setup shell

;; Note: Emacs runs .bashrc in *shell*

;; bash-completion

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
          'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
          'bash-completion-dynamic-complete)

;; tab-completion for shell-command

(require 'shell-command)
(shell-command-completion-mode)

(provide 'setup-shell)
