# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --extras='*' --with-list-header --list-extras
${CTAGS} --quiet --options=NONE --extras='*' --with-list-header --machinable --list-extras
