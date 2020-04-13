# Copyright: 2020 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1

V=
# V=valgrind

echo '# no warning should be printed' 1>&2
$CTAGS --quiet --options=NONE -o - input.sql
