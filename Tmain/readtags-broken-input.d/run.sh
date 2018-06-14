#!/bin/sh

# Copyright: 2018 Masatake YAMATO
# License: GPL-2

READTAGS=$3
#V="valgrind --leak-check=full --track-origins=yes -v"
V=

. ../utils.sh

if ! [ -x "${READTAGS}" ]; then
	skip "no readtags"
fi

for i in 1 2 3 4 5 6; do
	${V} ${READTAGS} -t ./target.tags - greet${i} > /dev/null || exit 1
done

${READTAGS} -t ./target.tags - greetA
${READTAGS} -t ./target.tags - greetB
