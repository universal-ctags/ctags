# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

O=/tmp/ctags-tmain-$$

echo '#' tags regular file
"${CTAGS}" --options=NONE --output-format=u-ctags -o $O --list-extras | grep pseudo

echo '#' tags -
"${CTAGS}" --options=NONE --output-format=u-ctags -o - --list-extras | grep pseudo

echo '#' tags NOTHING
"${CTAGS}" --options=NONE --output-format=u-ctags --list-extras | grep pseudo

echo '#' xref regular file
"${CTAGS}" --options=NONE --output-format=xref -o $O --list-extras | grep pseudo

echo '#' xref -
"${CTAGS}" --options=NONE --output-format=xref -o - --list-extras | grep pseudo

echo '#' xref NOTHING
"${CTAGS}" --options=NONE --output-format=xref --list-extras | grep pseudo

rm -f $O
