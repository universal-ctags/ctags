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

: &&
echo '# -en -l' &&
${V} ${READTAGS} -en -t ./output.tags -l &&

echo '# -enE -l' &&
${V} ${READTAGS} -enE -t ./output.tags -l &&

echo '# -en --escape-output -l' &&
${V} ${READTAGS} -en --escape-output -t ./output.tags -l &&

echo '# -en -D' &&
${V} ${READTAGS} -en -t ./output.tags -D &&

echo '# -en -E -D' &&
${V} ${READTAGS} -en -E -t ./output.tags -D &&

echo '# -en --escape-output -D' &&
${V} ${READTAGS} -en --escape-output -t ./output.tags -D &&

echo '# -en -l (output2)' &&
${V} ${READTAGS} -en -t ./output2.tags -l &&

echo '# -en --escape-output -l (output2)' &&
${V} ${READTAGS} -en --escape-output -t ./output2.tags -l &&
:
