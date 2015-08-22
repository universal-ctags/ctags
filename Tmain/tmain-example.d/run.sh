CTAGS=$1
DATADIR=$2
LIBEXECDIR=$3
BUILDDIR=$4

${CTAGS} --quiet --options=NONE --list-kinds=Ruby && echo X > ${BUILDDIR}/tags
