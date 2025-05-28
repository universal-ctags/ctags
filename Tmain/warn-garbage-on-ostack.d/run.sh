# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

$CTAGS --quiet --options=NONE --options=./garbage.ctags -o - input.garbage

$CTAGS --quiet --options=NONE --map-Kconfig=+.kconfig -o - input.kconfig > /dev/null
