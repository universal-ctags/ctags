# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --kinds-C=+'{label}' --list-kinds-full=C | grep label
${CTAGS} --quiet --options=NONE --kinds-C=-'{macro}' --list-kinds-full=C | grep macro
