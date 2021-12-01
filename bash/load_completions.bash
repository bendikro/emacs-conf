# -*- mode: shell-script -*-

OS=`uname`

if [[ "$OS" == 'FreeBSD' ]]; then
	if [ -f /usr/local/share/bash-completion/bash_completion.sh ]; then
		source /usr/local/share/bash-completion/bash_completion.sh
	fi
fi

for filename in $(find $EMACS_HOME/.emacs.d/bash/bash_completion/ -type f); do
	. $filename
done

if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /etc/bash_completion
fi

# Due to lazy loading, git has to be loaded explicitly here
# If not loaded here, the git completion in .emacs.d/bash/bash_completion/
# prevents the global git completion from being loaded
if [[ "$OS" != 'FreeBSD' ]]; then
	_completion_loader git
fi
