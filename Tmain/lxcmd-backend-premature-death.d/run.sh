# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
exit_if_no_coproc $CTAGS

chmod u+x ./foo.sh
$CTAGS --quiet --options=NONE --langdef=foo --xcmd-foo=./foo.sh -o - ./run.sh
