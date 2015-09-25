# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --options=NONE --options=./args.ctags --_force-quit=0
exit $?
