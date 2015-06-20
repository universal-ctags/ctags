CTAGS=$1
${CTAGS} --quiet --options=NONE --kinds-c=+l-f --list-kinds=C | grep '^[fl]'
