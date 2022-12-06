# Copyright: 2022 Masatake YAMATO
# License: GPL-2

CTAGS=$1

$CTAGS --quiet --options=NONE --list-fields=NONE | grep -v xpath
