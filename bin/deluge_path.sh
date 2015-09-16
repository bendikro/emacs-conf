#!/usr/bin/env bash
#set -x
PYPATH=`get_deluge_path.py`
export PYTHONPATH=$PYPATH
exec /usr/bin/env bash
