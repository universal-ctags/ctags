CTAGS=$1

${CTAGS} --quiet --options=NONE --xcmd-nosuchlang=./run.sh
exit $?
