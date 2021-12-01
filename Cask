;; This cask files is the main version of the main Cask file
(source gnu)
(source melpa)
(source org)

(package "emacs-conf" "0" "Python packages required for emacs-conf")

;; Latest signature key
(depends-on "gnu-elpa-keyring-update")

(depends-on "use-package")

(depends-on "browse-kill-ring")
(depends-on "cask")
(depends-on "flycheck")
(depends-on "flycheck-color-mode-line")
(depends-on "pallet")
(depends-on "yasnippet")
(depends-on "editorconfig")

;; Problems with magit on < 24.3
(depends-on "magit")

;; Utils
(depends-on "ggtags")
(depends-on "xclip" :git "https://github.com/emacsmirror/xclip")
(depends-on "exec-path-from-shell")
(depends-on "visual-regexp")
(depends-on "rainbow-mode")

;; File mode
(depends-on "apache-mode")
(depends-on "go-mode")
(depends-on "yaml-mode")
(depends-on "psgml" :git "https://git.code.sf.net/p/psgml/code" :ref "v1.3.2") ;; XML, HTML, markup langs ++

(depends-on "markdown-mode")
(depends-on "dockerfile-mode")
(depends-on "jinja2-mode")
(depends-on "dtrt-indent")
(depends-on "toml-mode")
(depends-on "json-mode")
(depends-on "nginx-mode")
(depends-on "origami") ;; Collapse code

;; Python
(depends-on "python-mode")
(depends-on "python-pylint" :git "https://github.com/emacsmirror/python-pylint")
(depends-on "elpy")
(depends-on "jedi")
(depends-on "yapfify")

;; LaTeX
(depends-on "auctex" :git "https://git.savannah.gnu.org/git/auctex.git" :ref "release_12_2")
(depends-on "auto-complete")

;; Lang
(depends-on "php-mode")
