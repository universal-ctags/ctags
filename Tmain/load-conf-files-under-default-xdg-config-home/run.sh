CTAGS=$1

. ../utils.sh

exit_if_win32 "$CTAGS"

export HOME=./myhome
export XDG_CONFIG_HOME=
${CTAGS}
