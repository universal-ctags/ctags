CTAGS=$1

chmod u+x ./foo.sh
$CTAGS --quiet --options=NONE --langdef=foo'{fileKind=!}' --xcmd-foo=./foo.sh -o - ./run.sh

