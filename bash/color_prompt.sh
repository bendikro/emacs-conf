if [[ "$ENABLE_PROMPT_COLORS" == 'true' ]]; then

	# ANSI color codes
	ANIS_RS="\[\033[0m\]"    # reset
	ANIS_HC="\[\033[1m\]"    # hicolor
	ANIS_UL="\[\033[4m\]"    # underline
	ANIS_INV="\[\033[7m\]"   # inverse background and foreground

	# Foreground
	ANIS_FG_BLK="\[\033[30m\]" # black
	ANIS_FG_RED="\[\033[31m\]" # red
	ANIS_FG_GRN="\[\033[32m\]" # green
	ANIS_FG_YEL="\[\033[33m\]" # yellow
	ANIS_FG_BLE="\[\033[34m\]" # blue
	ANIS_FG_MAG="\[\033[35m\]" # magenta
	ANIS_FG_CYN="\[\033[36m\]" # cyan
	ANIS_FG_WHT="\[\033[37m\]" # white

	# Bright
	ANSI_FG_BR_GRN="\[\033[01;32m\]" # bright green
	ANSI_FG_BR_BLE="\[\033[01;34m\]" # bright blue

	# Background
	ANIS_BG_BLK="\[\033[40m\]" # black
	ANIS_BG_RED="\[\033[41m\]" # red
	ANIS_BG_GRN="\[\033[42m\]" # green
	ANIS_BG_YEL="\[\033[43m\]" # yellow
	ANIS_BG_BLE="\[\033[44m\]" # blue
	ANIS_BG_MAG="\[\033[45m\]" # magenta
	ANIS_BG_CYN="\[\033[46m\]" # cyan
	ANIS_BG_WHT="\[\033[47m\]" # white

	PROMPT_LOGIN_COLOR=$ANSI_FG_BR_GRN
	PROMPT_CWD_COLOR=$ANSI_FG_BR_BLE

	PS1=$(printf "%s\\\\u@\H:%s\w%s\$ " "$PROMPT_LOGIN_COLOR" "$PROMPT_CWD_COLOR" "$ANIS_RS")
	export PS1=$PS1
fi
