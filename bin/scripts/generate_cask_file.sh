#!/usr/bin/env bash

# Generate the Cask config according to input type

# Arg1: The type
# Arg2: Path to the Cask file to modify

if [ $# -ne 2 ]
  then
    echo "There should be 2 arguments supplied, not $#"
    exit 1
fi

CASK_TYPE=$1
CASK_FILE=$2

unamestr=$(uname)

if [ "$unamestr" = 'Linux' ]; then
    SED_CMD=$(which sed)

elif [ "$unamestr" = 'FreeBSD' ]; then
    SED_CMD=$(which gsed)
    if [ "$SED_CMD" == "" ]; then
        echo "gsed binary not found!"
        exit 1
    fi
fi

echo "Using sed binary: $SED_CMD"

EMACS_MAJOR_VERSION=$(emacs --version | awk -F " " 'NR==1 {print $3}' | cut -d '.' -f 1)
echo "emacs_version: $EMACS_MAJOR_VERSION"

if [ "$CASK_TYPE" == "Ubuntu"  ]; then
  $SED_CMD -i -E -r '/HEADER/ s/^.*$/;; This cask file is Ubuntu/g' $CASK_FILE


elif [ "$CASK_TYPE" == "Ubuntu_current_emacs"  ]; then

  if [ "$EMACS_MAJOR_VERSION" == "25"  ]; then
    $SED_CMD -i -E -r "/HEADER/ s/^.*$/;; This cask file is for Ubuntu with emacs version $EMACS_MAJOR_VERSION/g" $CASK_FILE

    $SED_CMD -i -E -r '/gnu-elpa-keyring-update/ s/^(.*)$/;;\1/g' $CASK_FILE

    $SED_CMD -i -E -r '/auctex/ s/^(.*)$/;;\1/g' $CASK_FILE
    #$SED_CMD -i '\|auctex|a (depends-on "auctex" :git "https://git.savannah.gnu.org/git/auctex.git" :ref "release_12_2")' $CASK_FILE

    $SED_CMD -i -E -r '/browse-kill-ring/ s/^(.*)$/;;\1/g' $CASK_FILE

    $SED_CMD -i -E -r '/markdown-mode/ s/^(.*)$/;;\1/g' $CASK_FILE
    $SED_CMD -i '\|markdown-mode|a (depends-on "markdown-mode" :git "https://github.com/jrblevin/markdown-mode" :ref "v2.5")' $CASK_FILE

    $SED_CMD -i -E -r '/dtrt-indent/ s/^(.*)$/;;\1/g' $CASK_FILE
    $SED_CMD -i -E -r '/elpy/ s/^(.*)$/;;\1/g' $CASK_FILE
    $SED_CMD -i -E -r '/jedi/ s/^(.*)$/;;\1/g' $CASK_FILE
    $SED_CMD -i -E -r '/yapfify/ s/^(.*)$/;;\1/g' $CASK_FILE
    $SED_CMD -i -E -r '/nhexl-mode/ s/^(.*)$/;;\1/g' $CASK_FILE
    $SED_CMD -i -E -r '/go-mode/ s/^(.*)$/;;\1/g' $CASK_FILE # Requires 26
    $SED_CMD -i -E -r '/rainbow-mode/ s/^(.*)$/;;\1/g' $CASK_FILE
    $SED_CMD -i -E -r '/editorconfig/ s/^(.*)$/;;\1/g' $CASK_FILE
    $SED_CMD -i -E -r '/magit/ s/^(.*)$/;;\1/g' $CASK_FILE

    $SED_CMD -i -E -r '/ggtags/ s/^(.*)$/;;\1/g' $CASK_FILE
    $SED_CMD -i '\|ggtags|a (depends-on "ggtags" :git "https://github.com/leoliu/ggtags" :ref "0.8.13")' $CASK_FILE

    $SED_CMD -i -E -r '/psgml/ s/^(.*)$/;;\1/g' $CASK_FILE
    $SED_CMD -i '\|psgml|a (depends-on "psgml" :git "https://git.code.sf.net/p/psgml/code" :ref "v1.3.2") ;; XML, HTML, markup langs ++' $CASK_FILE

  fi

elif [ "$CASK_TYPE" == "FreeBSD"  ]; then
    $SED_CMD -i -E -r '/HEADER/ s/^.*$/;; This cask file is FreeBSD/g' $CASK_FILE
    $SED_CMD -i -E -r '/gnu-elpa-keyring-update/ s/^(.*)$/;;\1/g' $CASK_FILE
fi

echo "Generated Cask config for '$CASK_TYPE'"
