# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --machinable --with-list-header --list-kinds-full=C
${CTAGS} --quiet --options=NONE --machinable --with-list-header --list-kinds-full=C++

