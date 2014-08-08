#!/bin/sh
# note: notify-send is required, see libnotify-bin

# XXX do not notify if notification source has focus

delay="2000"

read line
summary="$line"
read line
msg="$line"
read line

#echo "summary:$summary"
#echo "msg:$msg"

if [ "$line" = "" ] && [ "$summary" != "" ]; then
    # Change the icon
    #[ -x "$(which notify-send)" ] && notify-send -i gtk-dialog-info -t "$delay" -- "$summary" "$msg"
    [ -x "$(which notify-send)" ] && sudo -u bro notify-send -i notification-message-im -t "$delay" -- "$summary" "$msg"
	echo "'Summary:$summary, Message:$msg'" >> ~/irssi_messages.txt
fi
