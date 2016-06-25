# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --_fatal-warnings -o - input.json
exit $?
