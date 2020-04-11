#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

READTAGS=$3
#V="valgrind --leak-check=full --track-origins=yes -v"
V=

. ../utils.sh

if ! [ -x "${READTAGS}" ]; then
	skip "no readtags"
fi

echo '# no -e option' &&
${V} ${READTAGS} -t ./sorted.tags -l &&

echo '# with -e option' &&
${V} ${READTAGS} -e -t ./sorted.tags -l &&

echo '# with -e -n option' &&
${V} ${READTAGS} -e -n -t ./sorted.tags -l
