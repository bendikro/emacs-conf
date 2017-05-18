## Setup from cloned repo


### Setup loading custom configs

```
~/.emacs.d$ ./bin/setup_config.py config tmux
```

#### Tmux

Install tmux plugins with "prefix + I"

### Cask

Install cask from home root:
```
$ curl -fsSkL https://raw.github.com/cask/cask/master/go | python`
```

Download packages:
```
~/.emacs.d$ cask install
```

## Clone repo and setup with curl

```
curl -fsSkL https://raw.githubusercontent.com/bendikro/emacs-conf/master/install | python
```

## Support for golang tags

```
git clone https://github.com/bendikro/gotags.git
cd gotags
rake install
```

Create TAGS file for a go project:
```
$ gotags
```
