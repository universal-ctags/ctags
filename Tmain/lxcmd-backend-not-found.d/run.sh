CTAGS=$1

source ../utils.sh
exit_if_no_coproc ${CTAGS}

chmod u+x ./foo.sh
$CTAGS --quiet --options=NONE --langdef=foo --xcmd-foo=./foo.sh -o - ./run.sh
