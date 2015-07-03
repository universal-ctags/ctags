CTAGS=$1
DATADIR=$2
LIBEXECDIR=$3

${CTAGS} --quiet --options=NONE --list-kinds=Ruby && echo X > tags
