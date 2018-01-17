#!/bin/bash

usage="$(basename "$0") [-h] [-s | -k | -l | -p] -- Handle docker ssh agent

where:
    -h  show this help text
    -s  start agent
    -k  kill agent
    -l  load agent environment
    -q  quiet
    -p  print environment
"

if [ "$#" -eq "0" ]; then
	echo "$usage"
fi

if [ -z "$DOCKER_AGENT_SOCK_DIR" ]; then
	echo "DOCKER_AGENT_SOCK_DIR environment variable is empty!. Please set it!"
fi

mkdir -p $DOCKER_AGENT_SOCK_DIR

SOCK="$DOCKER_AGENT_SOCK_DIR/docker-agent.sock"
AGENT_ENV="$DOCKER_AGENT_SOCK_DIR/docker-agent.env"
_AGENT_SILENT=${DOCKER_AGENT_LOAD_SILENT:-false}


load_docker_ssh_agent_env () {
	eval $(cat $AGENT_ENV)
}

start_docker_ssh_agent () {
	if [ -e "$SOCK" ]; then
		echo "DOCKER SSH AGENT SOCK already exist at: $SOCK"
	else
		ssh-agent -a $SOCK > $AGENT_ENV
		sed -i 's/SSH_AUTH_SOCK/DOCKER_SSH_AUTH_SOCK/g' $AGENT_ENV
		sed -i 's/SSH_AGENT_PID/DOCKER_SSH_AGENT_PID/g' $AGENT_ENV
		sed -i '/echo Agent pid.*/d' $AGENT_ENV
		load_docker_ssh_agent_env
		DOCKER_SSH_AUTH_SOCK_DIR=$(dirname $DOCKER_SSH_AUTH_SOCK)
		echo "export DOCKER_SSH_AUTH_SOCK_DIR=$DOCKER_SSH_AUTH_SOCK_DIR" >> $AGENT_ENV
		echo "docker ssh-agent started with pid $DOCKER_SSH_AGENT_PID"
	fi
}

while getopts ":sklph" opt; do
	case $opt in
		s)
			start_docker_ssh_agent
			;;
		k)
			load_docker_ssh_agent_env
			echo "Killing ssh-agent with pid $DOCKER_SSH_AGENT_PID"
			kill $DOCKER_SSH_AGENT_PID
			ret=$?
			if [ $ret -ne 0 ]; then
				echo "Failed to kill docker ssh-agent. Exit code: $ret"
			else
				rm -f $AGENT_ENV
			fi
			;;
		l)
			if [ ! -e "$AGENT_ENV" ]; then
				if [ "$_AGENT_SILENT" == "false" ]; then
					echo "Docker ssh-agent environment not found at $AGENT_ENV"
				fi
			else
				load_docker_ssh_agent_env
			fi
			;;
		p)
			if [ ! -e "$AGENT_ENV" ]; then
				echo "Docker ssh-agent environment not found at $AGENT_ENV"
			else
				cat $AGENT_ENV
				load_docker_ssh_agent_env
				kill -0 $DOCKER_SSH_AGENT_PID 2> /dev/null
				if [ $? -ne 0 ]; then
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
	esac
done
