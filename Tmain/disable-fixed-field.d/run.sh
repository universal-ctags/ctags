# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --fields=-N -o - input.c
${CTAGS} --quiet --options=NONE --fields=-F -o - input.c
${CTAGS} --quiet --options=NONE --fields=-P -o - input.c
${CTAGS} --quiet --options=NONE --fields=-'{name}' -o - input.c
${CTAGS} --quiet --options=NONE --fields=-'{input}' -o - input.c
${CTAGS} --quiet --options=NONE --fields=-'{pattern}' -o - input.c
