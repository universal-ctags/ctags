# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --fields=-N --_force-quit
${CTAGS} --quiet --options=NONE --fields=-F --_force-quit
${CTAGS} --quiet --options=NONE --fields=-P --_force-quit
${CTAGS} --quiet --options=NONE --fields=-'{name}' --_force-quit
${CTAGS} --quiet --options=NONE --fields=-'{input}' --_force-quit
${CTAGS} --quiet --options=NONE --fields=-'{pattern}' --_force-quit
