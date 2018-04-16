# -*- mode: shell-script -*-

OS=`uname`

if [[ "$OS" == 'FreeBSD' ]]; then
	if [ -f /usr/local/share/bash-completion/bash_completion.sh ]; then
		source /usr/local/share/bash-completion/bash_completion.sh
	fi
fi

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

for filename in $(find ~/.emacs.d/bash/bash_completion/ -type f); do
	. $filename
done
