# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

test 0 = $( ${CTAGS} --options=NONE --list-languages | grep -v "^[A-Z].*" | wc -l )
