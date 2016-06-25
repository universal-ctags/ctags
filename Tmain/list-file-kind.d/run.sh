# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

$1 --quiet --options=NONE --list-file-kind=Ruby \
    && $1 --quiet --options=NONE --list-file-kind=C

