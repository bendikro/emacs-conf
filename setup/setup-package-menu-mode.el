;; Modify with M-x set-variable package-menu-column-width
(defcustom package-menu-column-width 35
  "Width of the package column."
  :type 'number
  :group 'package)

(define-derived-mode package-menu-mode tabulated-list-mode "Package Menu"
    "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
	(setq tabulated-list-format
		  `[("Package" ,package-menu-column-width package-menu--name-predicate)
			("Version" 12 nil)
			("Status"  10 package-menu--status-predicate)
			,@(if (cdr package-archives)
				  '(("Archive" 10 package-menu--archive-predicate)))
			("Description" 0 nil)])
	(setq tabulated-list-padding 2)
	(setq tabulated-list-sort-key (cons "Status" nil))
	(add-hook 'tabulated-list-revert-hook 'package-menu--refresh nil t)
	(tabulated-list-init-header))

(message "SDKROOOOOO")

(provide 'setup-package-menu-mode)
