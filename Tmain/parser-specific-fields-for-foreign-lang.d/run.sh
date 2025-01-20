# Copyright: 2024 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1

V=
# V=valgrind

${V} ${CTAGS} --options=NONE --options=./knownz.ctags --options=./unknownx.ctags \
	 --fields=+l \
	 --fields-unknownx=+'{protection}{signature}' \
	 --fields-knownz=+'{owner}' \
	 -o - input.unknownx
