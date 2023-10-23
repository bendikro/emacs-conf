## Setup from cloned repo


### Setup loading custom configs

```
~/.emacs.d$ ./bin/setup_config.py [--desktop]
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

## Setup emacs for a user using another users install

```
EMACS_HOME=/home/otheruser /home/otheruser/.emacs.d/bin/setup_config.py config tmux
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
