CTAGS=$1

${CTAGS} --quiet --options=NONE --regex-nosuchlang=/a/
exit $?
