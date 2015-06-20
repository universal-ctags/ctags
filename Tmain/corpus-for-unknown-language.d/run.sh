CTAGS=$1

${CTAGS} --quiet --options=NONE --corpus-nosuchlang=Z:data
exit $?
