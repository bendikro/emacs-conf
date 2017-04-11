# -*- mode: shell-script -*-

# Print the number of newlines in the shell
function newlines() {
    count=10
    if [ $# -eq 1 ]; then
        count=$1
    fi
    for ((c=1; c <= $count; c++)); do
        echo ""
    done
}

function get_sessionname_func {
	PPID2=$PPID
	o=`screen -ls | grep $PPID2 | awk '{print $1}' | cut -f2 -d'.'`
	#o=`screen -ls | grep $PPID2`
	#echo -n "\005{= kG}[ $o ]\005{-}"
	echo -n "[ $o ]"
}

function grep_kill() {
    ret=`ps aux | grep $@`
    array=($ret);
    echo "Found: $ret"
    echo "Killing pid: ${array[1]}";
    kill -9 ${array[1]};
}


pathadd () {
	if ! echo "$PATH" | /bin/grep -Eq "(^|:)$1($|:)" ; then
		if [ "$2" = "after" ] ; then
			PATH="$PATH:$1"
		else
			PATH="$1:$PATH"
		fi
	fi
}

##########
# Docker

dbash() {
	docker exec -i -t $1 script -q -c "/bin/bash"
}


completions_dir=/usr/share/bash-completion/completions

if [ ! -d "$completions_dir" ]; then
	completions_dir=/usr/local/share/bash-completion/completions
fi

if [ -f "${completions_dir}/docker" ]; then
	source ${completions_dir}/docker

	_dbash_complete_containers() {
		COMPREPLY=()
		local cur prev words cword
		_get_comp_words_by_ref -n : cur prev words cword
		__docker_complete_containers_running
	}
	complete -F _dbash_complete_containers dbash
fi
