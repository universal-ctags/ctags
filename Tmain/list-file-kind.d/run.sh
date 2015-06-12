CTAGS=$1

$1 --quiet --options=NONE --list-file-kind=Ruby \
    && $1 --quiet --options=NONE --list-file-kind=C

