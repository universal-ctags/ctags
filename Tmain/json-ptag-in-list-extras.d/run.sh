# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1


. ../utils.sh

is_feature_available ${CTAGS} json

O=/tmp/ctags-tmain-$$

echo '#' json regular file
"${CTAGS}" --quiet --options=NONE --output-format=json -o $O --list-extras | grep pseudo

echo '#' json -
"${CTAGS}" --quiet --options=NONE --output-format=json -o - --list-extras | grep pseudo

echo '#' json NOTHING
"${CTAGS}" --quiet --options=NONE --output-format=json --list-extras | grep pseudo

rm -f $O
