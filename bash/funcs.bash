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
	ret=`ps aux | grep $1 | grep -v -e "grep"`
	ret_count=`ps aux | grep -v -e "grep" | grep -c $1`

	kill_signal="-15"
	if [ ! -z "$2" ]; then
		kill_signal=$2
	fi

	if [ "$ret_count" = 0 ]; then
		echo "No matches found for expression $1:"
	elif [ "$ret_count" = 1 ]; then
		process_array=($ret);
		echo "Killing pid: ${process_array[1]} with signal $kill_signal";
		kill $kill_signal ${process_array[1]};
	else
		echo "Too many matches ($ret_count) for expression $1:"
		printf "%b" "$ret\n"
	fi
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
# Vagrant


vagrantssh() {
	vagrant ssh `vagrant global-status | grep $1 | awk '{split($0,a," "); print a[1]}'`
}


##########
# Docker

dbash() {
	docker exec -i -t $1 script -q -c "/bin/bash"
}

docker-ip() {
	docker inspect --format '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$@"
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
	complete -F _dbash_complete_containers dbash docker-ip
fi
