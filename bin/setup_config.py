#!/usr/bin/env python

tmux = ".tmux.conf"
tmux_conf = """TMUX_CONF_DIR=%s/.emacs.d/configs/tmux
source-file $TMUX_CONF_DIR/tmux.conf
"""

bashrc = ".bashrc"
bashrc_conf = """if [ -f ~/.emacs.d/bash/bashrc_extras ]; then
    . ~/.emacs.d/bash/bashrc_extras
fi
"""

gitconfig = ".gitconfig"
gitconfig_conf = """[include]
     path = ~/.emacs.d/configs/gitconfig
"""

import re
import os.path
from os.path import expanduser

def add_to_config(filename, search_pattern, conf, create_if_not_exists=True):
    print "Updating '%s'" % filename
    if os.path.isfile(filename):
        with open(filename, 'r') as f:
            res = re.search(search_pattern, f.read(), flags=0)
            if res:
                return
    elif not create_if_not_exists:
        return

    with open(filename, 'a+') as f:
        f.write(conf)

if __name__ == '__main__':
    home = expanduser("~")

    tmux_conf_file = os.path.join(home, tmux)
    add_to_config(tmux_conf_file, "TMUX_CONF_DIR", tmux_conf % "/home/bro", True)

    bashrc_file = os.path.join(home, bashrc)
    add_to_config(bashrc_file, "/.emacs.d/bash/bashrc_extras", bashrc_conf, True)

    gitconfig_file = os.path.join(home, gitconfig)
    add_to_config(gitconfig_file, "/.emacs.d/configs/gitconfig", gitconfig_conf, True)
