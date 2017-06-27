# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo '## all|grep LdScript'
$CTAGS --quiet --options=NONE --list-map-extensions=all | grep '^#\|LdScript'

echo '## LdScript'
$CTAGS --quiet --options=NONE --list-map-extensions=LdScript
