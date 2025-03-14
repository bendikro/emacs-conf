# Clear screen properly
alias cl='printf "\033c"' # Clear screen and history
alias nl='newlines'
alias rlbrc='source ~/.bashrc'
alias em='emacs -nw'
alias emacs='XMODIFIERS= emacs'

alias tmuxr='tmux source-file ~/.tmux.conf'
alias cgrep='grep --include="*.c" --include="*.h"'
alias ccgrep='grep --include="*.c" --include="*.h" --include="*.cpp" --include="*.hpp"'
alias cppgrep='grep --include="*.cpp" --include="*.hpp"'
alias pygrep='grep --include="*.py"'
alias emgrep='grep --exclude-dir="backups" --exclude-dir="\.cask*" --exclude-dir="session-store" --exclude-dir="\.git"  --exclude-dir="\.python-environments" --exclude="*.eln"'

# Docker
alias dps="docker ps"
alias dpsi="docker images"
alias drun="docker run"
alias dstop="docker stop"


function setup_key_bindings {
# Only apply when we have a proper terminal
case "$TERM" in
    xterm*|rxvt*|screen*)
    # Bind CTRL-del to delete forward word
    bind '"\e[3;5~":kill-word'

	# CTRL+ left/right
    bind '"\e[1;5D":backward-word'
    bind '"\e[1;5C":forward-word'

    # turn off displaying ^C when pressing CTRL-C in terminal.
    stty -ctlecho
    ;;
esac
}


# Only setup key bindings when interactive
case $- in
    *i*) setup_key_bindings;;
      *) ;;
esac


# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

OS=`uname`
if [ "$OS" = "FreeBSD" ]; then
    alias ls='ls -G'
fi
