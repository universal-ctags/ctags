# Copyright: 2017 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1
# V=valgrind
echo '#' disabling fields
${V} ${CTAGS} --options=NONE --options=./unknown.ctags -o - input.unknown

echo '#' enabling signature only
${V} ${CTAGS} --options=NONE --options=./unknown.ctags --fields-unknown=+'{signature}' -o - input.unknown

echo '#' enabling protection only
${V} ${CTAGS} --options=NONE --options=./unknown.ctags --fields-unknown=+'{protection}' -o - input.unknown

echo '#' enabling both signature and protection
${V} ${CTAGS} --options=NONE --options=./unknown.ctags --fields-unknown=+'{protection}{signature}' -o - input.unknown
