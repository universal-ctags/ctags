# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

O=/tmp/ctags-tmain-$$.xref
$CTAGS --quiet --options=NONE --output-format=xref -f ${O} input.c > /dev/null && cat ${O}
s=$?

rm -f ${O}

exit $s
