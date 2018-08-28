# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --options=x.ctags -x --_xformat="%N %s %{end}" input.x
exit $?
