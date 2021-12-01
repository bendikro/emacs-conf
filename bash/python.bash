# virtualenvwrapper location on FreeBSD
if [ -f /usr/local/bin/virtualenvwrapper.sh ]; then
    . /usr/local/bin/virtualenvwrapper.sh
fi

# virtualenvwrapper location on Ubuntu 18/20
if [ -f /usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh ]; then
    . /usr/share/virtualenvwrapper/virtualenvwrapper_lazy.sh
fi

# Pymacs
export PYMACS_PYTHON=python3

# PYENV
if [ -d "$EMACS_HOME/.pyenv" ] ; then
   export PYENV_ROOT="$EMACS_HOME/.pyenv"
   pathadd $PYENV_ROOT/bin
   eval "$(pyenv init -)"
fi
