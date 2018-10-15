#!/bin/bash

__docker_agent_print_usage() {
	usage="$(basename "$0") [ -c | -h | -i | -k | -l | -p | -s | -a ssh-key] -- Control docker ssh agent

where:
    -a ssh-key  add key to agent
    -c          change environment to use docker ssh agent
    -h          show this help text
    -i          show identities
    -k          kill agent
    -l          load agent environment
    -p          print environment
    -s          start agent
"
	echo "$usage"
}


__docker_agent_mk_agent_sock_dir () {
	mkdir -p $DOCKER_SSH_AGENT_SOCK_DIR
}

__DOCKER_SOCK_FILE="docker-agent.sock"
__DOCKER_SOCK_FILE_PATH="$DOCKER_SSH_AGENT_SOCK_DIR/$__DOCKER_SOCK_FILE"
__DOCKER_AGENT_ENV="$DOCKER_SSH_AGENT_SOCK_DIR/docker-agent.env"
__DOCKER_AGENT_SILENT=${DOCKER_SSH_AGENT_LOAD_SILENT:-false}


__docker_agent_load_docker_ssh_agent_env () {
	eval $(cat $__DOCKER_AGENT_ENV)
}

__docker_agent_check_agent_running () {
	__docker_agent_load_docker_ssh_agent_env
	kill -0 $DOCKER_SSH_AGENT_PID 2> /dev/null
	if [ $? -ne 0 ]; then
		return 0
	fi
	return 1
}

__docker_agent_start_docker_ssh_agent () {
	__docker_agent_mk_agent_sock_dir
	if [ -e "$__DOCKER_SOCK_FILE_PATH" ]; then
		echo "DOCKER SSH AGENT SOCK already exist at: $__DOCKER_SOCK_FILE_PATH"
	else
		ssh-agent -a $__DOCKER_SOCK_FILE_PATH > $__DOCKER_AGENT_ENV
		sed -i 's/SSH_AUTH_SOCK/DOCKER_SSH_AUTH_SOCK/g' $__DOCKER_AGENT_ENV
		sed -i 's/SSH_AGENT_PID/DOCKER_SSH_AGENT_PID/g' $__DOCKER_AGENT_ENV
		sed -i '/echo Agent pid.*/d' $__DOCKER_AGENT_ENV
		__docker_agent_load_docker_ssh_agent_env
		DOCKER_SSH_AUTH_SOCK_DIR=$(dirname $DOCKER_SSH_AUTH_SOCK)
		DOCKER_SSH_AUTH_SOCK_FILE=$__DOCKER_SOCK_FILE
		echo "export DOCKER_SSH_AUTH_SOCK_DIR=$DOCKER_SSH_AUTH_SOCK_DIR" >> $__DOCKER_AGENT_ENV
		echo "export DOCKER_SSH_AUTH_SOCK_FILE=$DOCKER_SSH_AUTH_SOCK_FILE" >> $__DOCKER_AGENT_ENV
		echo "docker ssh-agent started with pid $DOCKER_SSH_AGENT_PID"
	fi
}

__docker_agent_print_docker_ssh_agent_env () {
	echo "Docker env file: $__DOCKER_AGENT_ENV"
	cat $__DOCKER_AGENT_ENV
	echo -e "\nIdentifies:"
	__docker_agent_list_identifies
}

__docker_agent_list_identifies () {
	__docker_agent_load_docker_ssh_agent_env
	SSH_AUTH_SOCK=$DOCKER_SSH_AUTH_SOCK
	ssh-add -l
}

# The only function to be used publically (i.e. directly from the shell)
docker_agent_activate() {
	echo "docker_agent_activate"
	if [ "bash" != "$0" ]; then
		echo "Run command in bash: \"source ${BASH_SOURCE[0]}; ${FUNCNAME[0]}\""
		exit 1;
	fi
	__docker_agent_load_docker_ssh_agent_env
	export SSH_AUTH_SOCK=$DOCKER_SSH_AUTH_SOCK
	export SSH_AGENT_PID=$DOCKER_SSH_AGENT_PID
	echo "Using ssh-agent at sock: $SSH_AUTH_SOCK with pid: $SSH_AGENT_PID"
}

__docker_agent_add_key_to_docker_ssh_agent () {
	local ssh_key="$1"
	__docker_agent_load_docker_ssh_agent_env
	SSH_AUTH_SOCK=$DOCKER_SSH_AUTH_SOCK
	ssh-add $ssh_key
}

__docker_agent_parse_args () {
	while getopts ":sklphica:" opt ${BASH_ARGV}; do
		case $opt in
			s)
				__docker_agent_start_docker_ssh_agent
				;;
			a)
				__docker_agent_add_key_to_docker_ssh_agent $OPTARG
				;;
			i)
				__docker_agent_list_identifies
				;;
			c)
				docker_agent_activate
				;;
			k)
				__docker_agent_load_docker_ssh_agent_env
				echo "Killing ssh-agent with pid $DOCKER_SSH_AGENT_PID"
				kill $DOCKER_SSH_AGENT_PID
				ret=$?
				if [ $ret -ne 0 ]; then
					echo "Failed to kill docker ssh-agent. Exit code: $ret"
				else
					rm -f $__DOCKER_AGENT_ENV
				fi
				;;
			l)
				if [ ! -e "$__DOCKER_AGENT_ENV" ]; then
					if [ "$__DOCKER_AGENT_SILENT" == "false" ]; then
						echo "Docker ssh-agent environment not found at $__DOCKER_AGENT_ENV"
					fi
				else
					__docker_agent_load_docker_ssh_agent_env
					__docker_agent_check_agent_running
					running=$?
					if [ $running -eq 0 ]; then
						echo "Docker ssh-agent with pid $DOCKER_SSH_AGENT_PID is not running!"
						echo "Start docker ssh-agent with: \"${BASH_SOURCE[0]} -s\""
					fi
				fi
				;;
			p)
				if [ ! -e "$__DOCKER_AGENT_ENV" ]; then
					echo "Docker ssh-agent environment not found at $__DOCKER_AGENT_ENV"
				else
					__docker_agent_print_docker_ssh_agent_env
					echo ""
					__docker_agent_check_agent_running
					running=$?
					if [ "$running" -eq "0" ]; then
						echo "Docker ssh-agent with pid $DOCKER_SSH_AGENT_PID is not running!"
					else
						echo "Docker ssh-agent is running with pid $DOCKER_SSH_AGENT_PID"
					fi
				fi
				;;
			h)
				__docker_agent_print_usage
				;;
			\?)
				echo "Invalid option: -$OPTARG" >&2
				;;
			:)
				echo "Option -$OPTARG requires an argument." >&2
				exit 1
				;;
		esac
	done
}

if [ -z "$DOCKER_SSH_AGENT_SOCK_DIR" ]; then
	echo "DOCKER_SSH_AGENT_SOCK_DIR environment variable is empty!. Please set it!";
	exit 1;
fi

# Script is not being sourced
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
	if [ "$#" -eq "0" ]; then
		__docker_agent_print_usage
	fi
	__docker_agent_parse_args
fi
