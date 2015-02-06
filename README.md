### Setup loading custon configs

#####tmux

Add the following line to ~/.tmux.conf
```
source-file ~/.emacs.d/configs/tmux.conf
```

#####bashrc
```
Add the following to ~/.bashrc
if [ -f ~/.emacs.d/bash/bashrc_extras ]; then
    . ~/.emacs.d/bash/bashrc_extras
fi
```
#####git

To load git config, add to ~/.gitconfig:
```
[include]
     path = ~/.emacs.d/configs/gitconfig
```

### Cask

Install cask from home root:
```
$ curl -fsSkL https://raw.github.com/cask/cask/master/go | python`
```

Download packages:
```
~/.emacs.d]$ cask install
```
