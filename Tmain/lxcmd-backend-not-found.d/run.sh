CTAGS=$1
chmod u+x ./foo.sh
$CTAGS --quiet --options=NONE --langdef=foo --xcmd-foo=./foo.sh -o - ./run.sh
