# Determines the first non-option word of the command line. This
# is usually the command
_supervisor_get_firstword() {
	local firstword i
	firstword=
	for ((i = 1; i < ${#COMP_WORDS[@]}; ++i)); do
		if [[ ${COMP_WORDS[i]} != -* ]]; then
			firstword=${COMP_WORDS[i]}
			break
			fi
		done

	echo $firstword
}

# Determines the first non-option word of the command line. This
# is usually the command
_supervisor_get_secondword() {
	local firstword i
	firstword=
	for ((i = 2; i < ${#COMP_WORDS[@]}; ++i)); do
		if [[ ${COMP_WORDS[i]} != -* ]]; then
			firstword=${COMP_WORDS[i]}
			break
			fi
		done

	echo $firstword
}

# Determines the last non-option word of the command line. This
# is usally a sub-command
_supervisor_get_lastword() {
	local lastword i
	lastword=
	for ((i = 1; i < ${#COMP_WORDS[@]}; ++i)); do
		if [[ ${COMP_WORDS[i]} != -* ]] && [[ -n ${COMP_WORDS[i]} ]] && [[ ${COMP_WORDS[i]} != $cur ]]; then
			lastword=${COMP_WORDS[i]}
			fi
		done
	echo $lastword
}

_supervisor_get_services() {
	local services=$(for x in `supervisorctl avail | awk '{print $1}'`; do echo ${x} ; done )
	echo $services
}

DEBUG_OUT=/root/superlog.txt


_supervisor_do_complete()
{
	# Either display words or options, depending on the user input
	if [[ $cur == -* ]]; then
		#echo -e "Completing complete_options: \"$complete_options\"" >> $DEBUG_OUT
		COMPREPLY=( $( compgen -W "$complete_options" -- $cur ))
	else
		#echo -e "Completing complete_words: \"$complete_words\"" >> $DEBUG_OUT
		COMPREPLY=( $( compgen -W "$complete_words" -- $cur ))
	fi
}

_supervisor()
{
	local cur prev opts base
	COMPREPLY=()
	cur="${COMP_WORDS[COMP_CWORD]}"
	prev="${COMP_WORDS[COMP_CWORD-1]}"
	firstword=$(_supervisor_get_firstword)
	lastword=$(_supervisor_get_lastword)

	#
	#  The basic options we'll complete.
	#
	GLOBAL_COMMANDS="add avail clear exit fg maintail open pid quit reload remove reread restart shutdown signal start status stop tail update version help"
	GLOBAL_COMMANDS_CASE="+(${GLOBAL_COMMANDS// /|})"
	complete_options=$GLOBAL_COMMANDS

	#echo -e "\n\n############\nprev = $prev, cur = $cur, firstword = $firstword, lastword = $lastword, COMP_CWORD: $COMP_CWORD, COMP_WORDS: $COMP_WORDS\n" >> $DEBUG_OUT

	GLOBAL_OPTIONS="\
		-c --configuration\
		-i --interactive\
		-s --serverurl\
		-u --username\
		-p --password\
		-r --history-file\
		-h --help"

	# Enable pattern lists like +(...|...) for case
	shopt -s extglob

	TAIL_OPTIONS="-f"

	SIGNAL_NAMES="HUP INT QUIT ILL TRAP ABRT BUS FPE KILL USR1 SEGV USR2 PIPE ALRM TERM STKFLT CHLD CONT STOP TSTP TTIN TTOU URG XCPU XFSZ VTALRM PROF WINCH POLL PWR SYS"
	SIGNAL_NAMES_CASE="+(${SIGNAL_NAMES// /|})"

	case "${cur}" in
		$GLOBAL_COMMANDS_CASE)
			complete_words="$GLOBAL_COMMANDS"
			_supervisor_do_complete
			return 0
			;;
	esac

	case "${firstword}" in
		tail)
			complete_options="$TAIL_OPTIONS"

			case "${prev}" in
				-f|tail)
					complete_words=$(_supervisor_get_services)
					;;
				*)
					complete_words=""
					;;
			esac
			;;
		signal)
			complete_words="$SIGNAL_NAMES"
			complete_options=""

			# Complete signal name if no space after name
			case "${cur}" in
				$SIGNAL_NAMES_CASE)
					complete_words="$SIGNAL_NAMES"
					_supervisor_do_complete
					return 0
					;;
			esac

			secondword=$(_supervisor_get_secondword)
			case "${secondword}" in
				$SIGNAL_NAMES_CASE)
					complete_words=$(_supervisor_get_services)
					;;
				*)
					complete_words=""
			esac
			;;
		start|stop|restart|status)

			case "${prev}" in
				all)
					# No point in completing more services
					complete_words=""
					;;
				*)
					complete_words="$(_supervisor_get_services) all"
					;;
			esac
			;;
		*)
			complete_words="$GLOBAL_COMMANDS"
			complete_options="$GLOBAL_OPTIONS"
		;;
	esac

	_supervisor_do_complete
   return 0
}
complete -F _supervisor supervisorctl
