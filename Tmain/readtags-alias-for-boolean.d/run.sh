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

if ! ( "${READTAGS}" -h | grep -q -e -Q ); then
    skip "no qualifier function in readtags"
fi

echo '!_ -Q #t point3d'
${V} ${READTAGS} -t output.tags -Q '#t' point3d

echo '!_ -Q ture point3d'
${V} ${READTAGS} -t output.tags -Q 'true' point3d

echo '!_ -Q #f point3d'
${V} ${READTAGS} -t output.tags -Q '#f' point3d

echo '!_ -Q false point3d'
${V} ${READTAGS} -t output.tags -Q 'false' point3d

echo '!_ -Q #t -l'
${V} ${READTAGS} -t output.tags -Q '#t' -l

echo '!_ -Q ture -l'
${V} ${READTAGS} -t output.tags -Q 'true' -l

echo '!_ -Q #f -l'
${V} ${READTAGS} -t output.tags -Q '#f' -l

echo '!_ -Q false -l'
${V} ${READTAGS} -t output.tags -Q 'false' -l
