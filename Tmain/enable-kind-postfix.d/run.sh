CTAGS=$1
${CTAGS} --quiet --options=NONE --c-kinds=+l-f --list-kinds=C | grep '^[fl]'



