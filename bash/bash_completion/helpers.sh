LOG_FILE="$HOME/bash_complete.log"

_bash_complete_log()
{
	echo "[${USER}][`date`] - ${*}" >> ${LOG_FILE}
}
