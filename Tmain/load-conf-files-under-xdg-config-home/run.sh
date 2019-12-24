CTAGS=$1

. ../utils.sh

exit_if_win32 "$CTAGS"

export XDG_CONFIG_HOME=./.config
${CTAGS}
