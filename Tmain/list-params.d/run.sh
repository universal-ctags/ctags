# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1
C="${CTAGS} --quiet --options=NONE"

# The layout of outputs are very different whether NewJava, a packcc
# based parser, is available or not. The stdout-expected.txt of
# this test case assumes the target ctags has the NewJava parser.
# Skip this test case if the target ctags doesn't have the parser,
. ../utils.sh
is_feature_available $CTAGS packcc

echo '# ALL'
${C} --with-list-header=yes --list-params
echo

echo '# ALL MACHINABLE'
${C} --with-list-header=yes --machinable --list-params
echo

echo '# ALL MACHINABLE NOHEADER'
${C} --with-list-header=no  --machinable --list-params
echo

echo '# CPP'
${C} --list-params=CPreProcessor
echo

echo '# CPP MACHINABLE'
${C} --with-list-header=yes --machinable --list-params=CPreProcessor
echo

echo '# CPP MACHINABLE NOHEADER'
${C} --with-list-header=no  --machinable --list-params=CPreProcessor
echo
