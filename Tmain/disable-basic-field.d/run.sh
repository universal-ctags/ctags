# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --options=NONE --fields=-N --_force-quit
${CTAGS} --options=NONE --fields=-F --_force-quit
${CTAGS} --options=NONE --fields=-P --_force-quit
