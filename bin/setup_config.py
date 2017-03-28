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

emacs_home = os.environ.get('EMACS_HOME', os.path.expanduser("~"))

conf_d = {"emacs_home": emacs_home,
          "home": home}

# Tuples with (Filename,  pattern,  conf)
configs = [
    # Tmux
    (os.path.join(home, ".tmux.conf"), "TMUX_CONF_DIR", """
TMUX_CONF_DIR=%(emacs_home)s/.emacs.d/configs/tmux
source-file $TMUX_CONF_DIR/tmux.conf
""" % conf_d, True),
    # .profile
    (os.path.join(home, ".profile"), "# Add pymacs to PYTHONPATH", """
# Add pymacs to PYTHONPATH
if [ -f %(emacs_home)s/.emacs.d/bash/pypath_pymacs ]; then
    . %(emacs_home)s/.emacs.d/bash/pypath_pymacs
fi
""" % conf_d, True),
    # .bashrc
(os.path.join(home, ".bashrc"), "/.emacs.d/bash/bashrc_extras", """
EMACS_HOME=%(emacs_home)s

if [ -f %(emacs_home)s/.emacs.d/bash/bashrc_extras ]; then
    . %(emacs_home)s/.emacs.d/bash/bashrc_extras
fi
""" % conf_d, True),
    # .bash_profile
(os.path.join(home, ".bash_profile"), "/.emacs.d/bash/bashrc_extras", """
# Needed for tmux
EMACS_HOME=%(emacs_home)s

if [ -f %(emacs_home)s/.emacs.d/bash/bashrc_extras ]; then
    . %(emacs_home)s/.emacs.d/bash/bashrc_extras
fi

. %(home)s/.bashrc

""" % conf_d, True),
    # inputrc
(os.path.join(home, ".inputrc"), "/.emacs.d/configs/inputrc", """
$include %(emacs_home)s/.emacs.d/configs/inputrc
""" % conf_d, True),
    # Git config
    (os.path.join(home, ".gitconfig"), "/.emacs.d/configs/gitconfig", """
[include]
     path = %(emacs_home)s/.emacs.d/configs/gitconfig
""" % conf_d, True),
    # gtk-3.0 active terminal tab color
    (os.path.join(home, ".config/gtk-3.0/gtk.css"), "TerminalWindow .notebook tab:active", """
TerminalWindow .notebook tab:active {
    background-color: #b6bccb;
}
""", "Ubuntu" in os_line)
]

if emacs_home != os.path.expanduser("~"):
    configs.append(
        # .emacs
        (os.path.join(home, ".emacs"), "# Use load emacs config from", """
;; Use load emacs config from %(emacs_home)s
(setq user-init-file "%(emacs_home)s/.emacs.d/init.el")
(defvar user-home-dir "%(emacs_home)s")
(setq user-emacs-directory "%(emacs_home)s/.emacs.d/")
(defvar user-writable-dir "%(home)s/.emacs.d/")
(load-file "%(emacs_home)s/.emacs.d/init.el")
""" % conf_d, True))


if __name__ == '__main__':

    if not ("config" in sys.argv or "tmux" in sys.argv):
        print "Valid arguments are 'config' and 'tmux'. Specify env EMACS_HOME to change the location of .emacs.d. Default to HOME."
        sys.exit()

    if "tmux" in sys.argv:
        tpm = os.path.join(home, ".tmux/plugins/tpm")
        if not os.path.isdir(tpm):
            os.makedirs(tpm)
            subprocess.call("git clone https://github.com/tmux-plugins/tpm %s" % tpm, shell=True)

    if "config" in sys.argv:
        for fname, pattern, conf, create_if_not_exists in configs:
            add_to_config(fname, pattern, conf, create_if_not_exists=create_if_not_exists)
