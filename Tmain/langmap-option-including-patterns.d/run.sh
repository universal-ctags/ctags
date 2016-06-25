# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

: &&
    echo 1. &&
    ${CTAGS} --quiet --options=NONE --langdef=foo --langmap=foo:'(akefile)' --langmap=foo:'([Mm]akefile)(GNUmakefile)' --list-maps | grep 'akefile' &&
    echo 2. &&
    ${CTAGS} --quiet --options=NONE --langmap=Ruby:+'([Mm]akefile)(GNUmakefile).html' --list-maps | grep '\.rb' &&
    echo 3. &&
    ${CTAGS} --quiet --options=NONE --langmap=Ruby:+'([Mm]akefile)(GNUmakefile).html' --list-maps | grep 'akefile' &&
    echo 4. &&
    ${CTAGS} --quiet --options=NONE --langmap=Ruby:+'([Mm]akefile)(GNUmakefile).html' --list-maps | grep '\.html' &&
    echo 5. &&
    ${CTAGS} --quiet --options=NONE --langmap=Ruby:+'([Mm]akefile)(GNUmakefile).html' --langmap=default --list-maps | grep '\.html'

exit $?
