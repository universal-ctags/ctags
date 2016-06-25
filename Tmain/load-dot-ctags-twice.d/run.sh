# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

# TODO: --quiet
${CTAGS} --options=NONE --options=./dot.ctags --options=./dot.ctags --_force-quit

