CTAGS=$1

${CTAGS} --quiet --options=NONE --map-nosuchlang=.Z
exit $?
