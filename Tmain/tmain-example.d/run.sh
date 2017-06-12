# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

${CTAGS} --quiet --options=NONE --list-kinds=Ruby && echo X > ${BUILDDIR}/tags
