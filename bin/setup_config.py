#!/usr/bin/env python

import os
import re
import subprocess
import sys


def check_output_copy(*popenargs, **kwargs):
    r"""Copy from https://hg.python.org/cpython/file/d37f963394aa/Lib/subprocess.py#l647
    Not available in python2.6
    """
    from subprocess import Popen, PIPE
    if 'stdout' in kwargs:
        raise ValueError('stdout argument not allowed, it will be overridden.')
    process = Popen(stdout=PIPE, *popenargs, **kwargs)
    output, unused_err = process.communicate()
    retcode = process.poll()
    if retcode:
        cmd = kwargs.get("args")
        if cmd is None:
            cmd = popenargs[0]
        raise CalledProcessError(retcode, cmd, output=output)
    return output

try:
    from subprocess import check_output
except:
    check_output = check_output_copy


def add_to_config(filename, search_pattern, conf, create_if_not_exists=True):
    print "Updating '%s'" % filename
    if os.path.isfile(filename):
        with open(filename, 'r') as f:
            res = re.search(search_pattern, f.read(), flags=0)
            if res:
                return
    elif not create_if_not_exists:
        print "Nothing done on '%s'" % filename
        return

    path_dir = os.path.dirname(filename)
    if not os.path.isdir(path_dir):
        print "Creating directory '%s'" % path_dir
        os.makedirs(path_dir)

    with open(filename, 'a+') as f:
        f.write(conf)

home = os.path.expanduser("~")
os_line = check_output(["uname", "-a"])

# Tuples with (Filename,  pattern,  conf)
configs = [
    # Tmux
    (os.path.join(home, ".tmux.conf"), "TMUX_CONF_DIR", """
TMUX_CONF_DIR=%s/.emacs.d/configs/tmux
source-file $TMUX_CONF_DIR/tmux.conf
""" % home, True),
    # .profile
    (os.path.join(home, ".profile"), "# Add pymacs to PYTHONPATH", """
# Add pymacs to PYTHONPATH
if [ -f $HOME/.emacs.d/bash/pypath_pymacs ]; then
    . $HOME/.emacs.d/bash/pypath_pymacs
fi
""", True),
    # bash
(os.path.join(home, ".bashrc"), "/.emacs.d/bash/bashrc_extras", """
if [ -f ~/.emacs.d/bash/bashrc_extras ]; then
    . ~/.emacs.d/bash/bashrc_extras
fi
""", True),
    # inputrc
(os.path.join(home, ".inputrc"), "/.emacs.d/configs/inputrc", """
$include ~/.emacs.d/configs/inputrc
""", True),
    # Git config
    (os.path.join(home, ".gitconfig"), "/.emacs.d/configs/gitconfig", """
[include]
     path = ~/.emacs.d/configs/gitconfig
""", True),
    # gtk-3.0 active terminal tab color
    (os.path.join(home, ".config/gtk-3.0/gtk.css"), "TerminalWindow .notebook tab:active", """
TerminalWindow .notebook tab:active {
    background-color: #b6bccb;
}
""", "Ubuntu" in os_line)
]


if __name__ == '__main__':

    if not ("config" in sys.argv or "tmux" in sys.argv):
        print "Valid arguments are 'config' and 'tmux'"
        sys.exit()

    if "tmux" in sys.argv:
        tpm = os.path.join(home, ".tmux/plugins/tpm")
        if not os.path.isdir(tpm):
            os.makedirs(tpm)
            subprocess.call("git clone https://github.com/tmux-plugins/tpm %s" % tpm, shell=True)

    if "config" in sys.argv:
        for fname, pattern, conf, create_if_not_exists in configs:
            add_to_config(fname, pattern, conf, create_if_not_exists=create_if_not_exists)
