#!/bin/bash

usage="$(basename "$0") [-h] [-s | -k | -l | -p | -a ssh-key] -- Handle docker ssh agent

where:
    -s  start agent
    -k  kill agent
    -l  load agent environment
    -p  print environment
    -a  add key to agent
    -i  show identities
    -h  show this help text
"

if [ "$#" -eq "0" ]; then
	echo "$usage"
fi

if [ -z "$DOCKER_SSH_AGENT_SOCK_DIR" ]; then
	echo "DOCKER_SSH_AGENT_SOCK_DIR environment variable is empty!. Please set it!"
fi

mkdir -p $DOCKER_SSH_AGENT_SOCK_DIR

__DOCKER_SOCK_FILE="docker-agent.sock"
__DOCKER_SOCK="$DOCKER_SSH_AGENT_SOCK_DIR/$__DOCKER_SOCK_FILE"
__DOCKER_AGENT_ENV="$DOCKER_SSH_AGENT_SOCK_DIR/docker-agent.env"
__DOCKER_AGENT_SILENT=${DOCKER_SSH_AGENT_LOAD_SILENT:-false}


load_docker_ssh_agent_env () {
	eval $(cat $__DOCKER_AGENT_ENV)
}

check_agent_running () {
	load_docker_ssh_agent_env
	kill -0 $DOCKER_SSH_AGENT_PID 2> /dev/null
	if [ $? -ne 0 ]; then
		return 0
	fi
	return 1
}

start_docker_ssh_agent () {
	if [ -e "$__DOCKER_SOCK" ]; then
		echo "DOCKER SSH AGENT SOCK already exist at: $__DOCKER_SOCK"
	else
		ssh-agent -a $__DOCKER_SOCK > $__DOCKER_AGENT_ENV
		sed -i 's/SSH_AUTH_SOCK/DOCKER_SSH_AUTH_SOCK/g' $__DOCKER_AGENT_ENV
		sed -i 's/SSH_AGENT_PID/DOCKER_SSH_AGENT_PID/g' $__DOCKER_AGENT_ENV
		sed -i '/echo Agent pid.*/d' $__DOCKER_AGENT_ENV
		load_docker_ssh_agent_env
		DOCKER_SSH_AUTH_SOCK_DIR=$(dirname $DOCKER_SSH_AUTH_SOCK)
		DOCKER_SSH_AUTH_SOCK_FILE=$__DOCKER_SOCK_FILE
		echo "export DOCKER_SSH_AUTH_SOCK_DIR=$DOCKER_SSH_AUTH_SOCK_DIR" >> $__DOCKER_AGENT_ENV
		echo "export DOCKER_SSH_AUTH_SOCK_FILE=$DOCKER_SSH_AUTH_SOCK_FILE" >> $__DOCKER_AGENT_ENV
		echo "docker ssh-agent started with pid $DOCKER_SSH_AGENT_PID"
	fi
}

print_docker_ssh_agent_env () {
	echo "Docker env file: $__DOCKER_AGENT_ENV"
	cat $__DOCKER_AGENT_ENV
	echo -e "\nIdentifies:"
	list_identifies
}

list_identifies () {
	load_docker_ssh_agent_env
	SSH_AUTH_SOCK=$DOCKER_SSH_AUTH_SOCK
	ssh-add -l
}

add_key_to_docker_ssh_agent () {
	local ssh_key="$1"
	load_docker_ssh_agent_env
	SSH_AUTH_SOCK=$DOCKER_SSH_AUTH_SOCK
	ssh-add $ssh_key
}

while getopts ":sklphia:" opt; do
	case $opt in
		s)
			start_docker_ssh_agent
			;;
		a)
			add_key_to_docker_ssh_agent $OPTARG
			;;
		i)
			list_identifies
			;;
		k)
			load_docker_ssh_agent_env
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
				load_docker_ssh_agent_env
				check_agent_running
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
				print_docker_ssh_agent_env
				echo ""
				check_agent_running
				running=$?
				if [ "$running" -eq "0" ]; then
					echo "Docker ssh-agent with pid $DOCKER_SSH_AGENT_PID is not running!"
				else
					echo "Docker ssh-agent is running with pid $DOCKER_SSH_AGENT_PID"
				fi
			fi
			;;
		h)
			echo "$usage"
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
