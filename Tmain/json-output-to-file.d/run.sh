# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

O=/tmp/ctags-tmain-$$.json

is_feature_available "${CTAGS}" json

$CTAGS --quiet --options=NONE --extras=-p --output-format=json  -f ${O} input.c > /dev/null && cat ${O}
s=$?

rm -f ${O}

exit $s
