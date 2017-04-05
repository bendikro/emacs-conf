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
