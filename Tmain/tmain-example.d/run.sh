# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1
LIBEXECDIR=$2
BUILDDIR=$3

${CTAGS} --quiet --options=NONE --list-kinds=Ruby && echo X > ${BUILDDIR}/tags
