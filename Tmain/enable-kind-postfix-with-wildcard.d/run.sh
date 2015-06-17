CTAGS=$1
${CTAGS} --quiet --options=NONE '--*-kinds=*' --list-kinds | grep '\[off\]'

