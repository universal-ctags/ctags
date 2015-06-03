CTAGS=$1
chmod u+x ./foo.sh
$CTAGS --langdef=foo --xcmd-foo=./foo.sh -o - ./run.sh
