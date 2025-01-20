# Copyright: 2017 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1

V=
# V=valgrind

echo '#' disabling fields
${V} ${CTAGS} --options=NONE --options=./unknownx.ctags -o - input.unknownx

echo '#' enabling signature only
${V} ${CTAGS} --options=NONE --options=./unknownx.ctags --fields-unknownx=+'{signature}' -o - input.unknownx

echo '#' enabling protection only
${V} ${CTAGS} --options=NONE --options=./unknownx.ctags --fields-unknownx=+'{protection}' -o - input.unknownx

echo '#' enabling both signature and protection
${V} ${CTAGS} --options=NONE --options=./unknownx.ctags --fields-unknownx=+'{protection}{signature}' -o - input.unknownx
