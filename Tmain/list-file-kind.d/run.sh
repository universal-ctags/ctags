CTAGS=$1

$1 --quiet --list-file-kind=Ruby \
    && $1 --quiet --list-file-kind=C

