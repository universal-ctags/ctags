# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --fields=-N --_force-quit
${CTAGS} --fields=-F --_force-quit
${CTAGS} --fields=-P --_force-quit
