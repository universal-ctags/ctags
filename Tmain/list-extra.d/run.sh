# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --extra='*' --with-list-header --list-extra
${CTAGS} --quiet --options=NONE --extra='*' --with-list-header --machinable --list-extra
