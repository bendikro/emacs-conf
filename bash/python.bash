
if [ -f "${VIRTUALENVWRAPPER_SCRIPT}" ]; then
  . ${VIRTUALENVWRAPPER_SCRIPT}
elif [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
  # virtualenvwrapper location on FreeBSD
  . /usr/local/bin/virtualenvwrapper.sh
elif [ -f /usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh ]; then
  # virtualenvwrapper location on Ubuntu 18/20
  . /usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh
fi

# Pymacs
export PYMACS_PYTHON=${PYMACS_PYTHON:-python3}

# PYENV
if [ -d "$EMACS_HOME/.pyenv" ] ; then
   export PYENV_ROOT="$EMACS_HOME/.pyenv"
   pathadd $PYENV_ROOT/bin
   eval "$(pyenv init -)"
fi
