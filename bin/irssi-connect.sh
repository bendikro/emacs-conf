#!/usr/bin/env bash

notify="$HOME/.emacs.d/bin/notify-remote.sh"

host="freebsd"
notify_port=12003
ssh_port=22

# If we get an argument, use it for ssh port, otherwise use default of 22
if [ -n "$1" ]; then
    ssh_port=$1
fi

ssh_cmd="'touch ~/.irssi/notify/$notify_port; tmux attach -t irc || tmux new-session -s irc'"
#echo "ssh_cmd:$ssh_cmd"

set -e
set -x

socat -u tcp4-listen:$notify_port,reuseaddr,fork,bind=127.0.0.1 exec:$notify &
socat_pid=$!

eval "ssh -o ServerAliveInterval=20 $host -p $ssh_port -R $notify_port:localhost:$notify_port -t $ssh_cmd"

echo "Killing socat: $socat_pid"
kill $socat_pid
