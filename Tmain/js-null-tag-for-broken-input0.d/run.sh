CTAGS=$1

${CTAGS} --quiet --options=NONE -o - input.js > /dev/null

exit $?
