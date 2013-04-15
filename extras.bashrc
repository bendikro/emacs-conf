# -*- mode: shell-script -*-

# Add to ~/.bashrc
#if [ -f ~/.emacs.d/bash_extras ]; then
#    . ~/.emacs.d/bash_extras
#fi

# Clear screen properly
alias cl='printf "\033c"'

# Git aliases
alias ga='git add'
alias gp='git push'
alias gl='git log'
alias gs='git status'
alias gd='git diff'
alias gdt='git difftool'
alias gco='git commit -m'
alias gcoa='git commit -am'
alias gb='git branch'
alias gc='git checkout'
alias gpu='git pull'
alias gr='git remote'
alias gh='git show'

#if [ -f /home/bendiko/programmer/git-contrib/git/contrib/completion/git-completion.bash ]; then
#    . /home/bendiko/programmer/git-contrib/git/contrib/completion/git-completion.bash
#    __git_complete gb _git_branch
#    __git_complete gc _git_checkout
#fi


# Bind CTRL-del to delete forward word
bind '"\e[3;5~":kill-word'

# turn off displaying ^C when pressing CTRL-C in terminal.
stty -ctlecho

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi