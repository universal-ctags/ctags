# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

: &&
    # Clear all mappings related to a language
    echo 1. &&
    ${CTAGS} --quiet --options=NONE --langmap=Ruby: --list-maps | grep '^Ruby' &&
    # Take over a mapping from the other language (resetting)
    echo 2. &&
    ${CTAGS} --quiet --options=NONE --langdef=foo --langmap=foo:.rb --list-maps | grep '\.rb' &&
    # Overwrite a mapping
    echo 3. &&
    ${CTAGS} --quiet --options=NONE --langdef=foo --langmap=foo:.zzz --langmap=foo:.html --list-maps | grep '\.html' &&
exit $?
