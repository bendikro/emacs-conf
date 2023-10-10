#!/usr/bin/env python3
from __future__ import print_function

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

    if sys.version_info >= (3,0):
        # On python3 check_output returns bytes type
        def check_output_py3(args, *argv, **kwargs):
            return check_output(args, **kwargs).decode("utf-8")

        check_proc_output = check_output_py3
except:
    check_proc_output = check_output_copy


def add_to_config(conf):
    """
    Create or update the config files
    """
    print("Updating '%s'" % conf['filepath'])
    if os.path.isfile(conf['filepath']) and conf['pattern']:
        with open(conf['filepath'], 'r') as f:
            res = re.search(conf['pattern'], f.read(), flags=0)
            if res:
                return
    elif not conf['create_if_not_exists']:
        print("Nothing done on '%s'" % conf['filepath'])
        return

    path_dir = os.path.dirname(conf['filepath'])
    if not os.path.isdir(path_dir):
        print("Creating directory '%s'" % path_dir)
        os.makedirs(path_dir)

    with open(conf['filepath'], 'a+') as f:
        f.write(conf['config'])


tilde_home = os.path.expanduser("~")
os_line = check_proc_output(["uname", "-a"])

emacs_home = os.environ.get('EMACS_HOME', tilde_home).rstrip('/')

print("Using emacs_home: '%s'" % emacs_home)

conf_d = {"emacs_home": emacs_home,
          "tilde_home": tilde_home}

configs = {}

def add_config(name, config, filepath=None, search_pattern=None, create_if_not_exists=True):
    if not filepath:
        filepath = os.path.join(tilde_home, name)
    configs[name] = {'filepath':  filepath, 'pattern': search_pattern, 'config': config % conf_d,
                     'create_if_not_exists': create_if_not_exists}

add_config('.tmux.conf',
"""
TMUX_CONF_DIR=%(emacs_home)s/.emacs.d/configs/tmux
source-file $TMUX_CONF_DIR/tmux.conf
""", search_pattern="TMUX_CONF_DIR")

add_config('.profile',
"""
# Add pymacs to PYTHONPATH
if [ -f %(emacs_home)s/.emacs.d/bash/pypath_pymacs ]; then
    . %(emacs_home)s/.emacs.d/bash/pypath_pymacs
fi
""", search_pattern="# Add pymacs to PYTHONPATH")

add_config('.bashrc',
"""
EMACS_HOME=%(emacs_home)s

if [ -f %(emacs_home)s/.emacs.d/bash/bashrc_extras ]; then
    . %(emacs_home)s/.emacs.d/bash/bashrc_extras
fi
""", search_pattern="/.emacs.d/bash/bashrc_extras")

add_config('.bash_profile',
"""
# Needed for tmux
EMACS_HOME=%(emacs_home)s

if [ -f %(emacs_home)s/.emacs.d/bash/bashrc_extras ]; then
    . %(emacs_home)s/.emacs.d/bash/bashrc_extras
fi

. %(tilde_home)s/.bashrc

""", search_pattern="/.emacs.d/bash/bashrc_extras")

add_config('.inputrc',
"""
$include %(emacs_home)s/.emacs.d/configs/inputrc
""", search_pattern="/.emacs.d/configs/inputrc")

add_config('.gitconfig',
"""
[include]
     path = %(emacs_home)s/.emacs.d/configs/gitconfig
""", search_pattern="/.emacs.d/configs/gitconfig")

add_config('.config/gtk-3.0/gtk.css',
"""
TerminalWindow .notebook tab:active {
    background-color: #b6bccb;
}

terminal-window .notebook tab:active {
    background-color: #b6bccb;
}

.terminator-terminal-window notebook header tab {
   background-color: shade(@bg_color, 0.8)
}
""", search_pattern="terminal-window .notebook tab:active", create_if_not_exists="Ubuntu" in os_line)


add_config('.config/htop/htoprc',
"""# Beware! This file is rewritten by htop when settings are changed in the interface.
# The parser is also very primitive, and not human-friendly.
fields=0 48 17 18 38 39 40 2 46 47 49 109 110 1
sort_key=46
sort_direction=1
hide_threads=0
hide_kernel_threads=1
hide_userland_threads=0
shadow_other_users=0
show_thread_names=0
show_program_path=0
highlight_base_name=0
highlight_megabytes=1
highlight_threads=1
tree_view=0
header_margin=1
detailed_cpu_time=0
cpu_count_from_zero=0
update_process_names=0
account_guest_in_cpu_meter=0
color_scheme=0
delay=15
left_meters=LeftCPUs Memory Swap
left_meter_modes=1 1 1
right_meters=RightCPUs Tasks LoadAverage Uptime
right_meter_modes=1 2 2 2
""")

add_config('.config/terminator/config',
"""[global_config]
[keybindings]
  broadcast_group = <Primary><Alt>g
  go_down = <Primary><Shift><Alt>Down
  go_right = <Primary><Shift><Alt>Right
  go_up = <Primary><Shift><Alt>Up
  resize_left = ISO_Level3_Shift
  resize_right = ISO_Level3_Shift
[profiles]
  [[default]]
    background_image = None
    scroll_on_output = False
    scrollback_lines = 30000
	custom_command = TERM=xterm-256color bash -l
    use_custom_command = True
""", search_pattern="custom_command", create_if_not_exists="Ubuntu" in os_line)


if emacs_home != tilde_home:
    print("EMACS_HOME (%s) and tilde_home (%s) differ. Adding .emacs file" % (emacs_home, tilde_home))
    add_config('.emacs',
"""
;; Use load emacs config from %(emacs_home)s
(setq user-init-file "%(emacs_home)s/.emacs.d/init.el")
(defvar user-home-dir "%(emacs_home)s")
(setq user-emacs-directory "%(emacs_home)s/.emacs.d/")
(defvar user-writable-dir "%(emacs_home)s/.emacs.d/")
(load-file "%(emacs_home)s/.emacs.d/init.el")
""", search_pattern="# Use load emacs config from")


DEFAULT_SERVER_CONFIGS = ['.tmux.conf', '.bashrc', '.gitconfig', '.config/htop/htoprc']
DEFAULT_DESKTOP_CONFIGS = ['.profile', '.bash_profile', '.config/gtk-3.0/gtk.css', '.config/terminator/config']


if __name__ == '__main__':
    import argparse
    import textwrap

    parser = argparse.ArgumentParser(
        prog="Linux config setup",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent('''\
        Environment variables:
            EMACS_HOME:
                Set to the path of the .emacs.d parent directory if it isn't "~/".
                This will write the file ~/.emacs which loads EMACS_HOME/.emacs.d/init.el
         '''))

    configs_to_write = DEFAULT_SERVER_CONFIGS

    parser.add_argument('--tmux-plugins', action='store_true', help="Clone the tmux plugins repo")
    parser.add_argument('--configs', dest='configs', help='Comma seperated list of configs to write. Default: "{}"'.format(", ".join(configs_to_write)))
    parser.add_argument('--desktop', action='store_true', help='Add the desktop configs: "%s"' % (", ".join(DEFAULT_DESKTOP_CONFIGS)))
    parser.add_argument('--inputrc', action='store_true', help='Add .inputrc file')

    args = parser.parse_args()

    if args.desktop:
        configs_to_write += DEFAULT_DESKTOP_CONFIGS
    if args.inputrc:
        configs_to_write += ['.inputrc']
    if args.configs:
        configs_to_write = [val.strip() for val in args.configs.split(',')]

    if args.tmux_plugins:
        tpm = os.path.join(tilde_home, ".tmux/plugins/tpm")
        if not os.path.isdir(tpm):
            os.makedirs(tpm)
            subprocess.call("git clone https://github.com/tmux-plugins/tpm %s" % tpm, shell=True)

    for conf_k in configs_to_write:
        try:
            add_to_config(configs[conf_k])
        except KeyError as err:
            print("Invalid config file:", err)
