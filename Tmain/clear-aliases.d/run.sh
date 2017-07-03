# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1
${CTAGS} --quiet --options=NONE --alias-C= --with-list-header=no --list-aliases=C
exit $?
