# Copyright: 2024 Masatake YAMATO
# License: GPL-2

CTAGS=$1

$CTAGS --quiet --options=NONE --machinable --with-list-header \
       --list-output-formats | grep -v '^json'
