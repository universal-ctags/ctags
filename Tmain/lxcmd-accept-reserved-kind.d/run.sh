CTAGS=$1

chmod u+x ./foo.sh
$CTAGS --langdef=foo'{fileKind=!}' --xcmd-foo=./foo.sh -o - ./run.sh

