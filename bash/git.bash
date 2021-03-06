############################
# GIT
############################

alias g='git'

# Git aliases
alias ga='git add'
alias gb='git branch'
alias gc='git checkout'
alias gcl='git clone'
alias gm='git commit -m'
alias gma='git commit -am'
alias gd='git diff'
alias gdt='git difftool'
alias gf='git fetch'
alias gl='git log'
alias gpu='git pull'
alias gp='git push'
alias gr='git remote'
alias gra='git remote add'
alias grr='git remote rm'
alias gsh='git show'
alias gs='git status'
alias gsu='git status --untracked-files=no'
alias gcma='git commit --amend --no-edit --reset-author'

# Set up bash completion for git commands
__git_complete gb _git_branch
__git_complete gc _git_checkout
__git_complete gd _git_diff
__git_complete gf _git_fetch
__git_complete gl _git_log
__git_complete gpu _git_pull
__git_complete gp _git_push
__git_complete gr _git_remote
__git_complete gsh _git_show
