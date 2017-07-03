#!/bin/sh

# Copyright: 2016 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
    skip "no readtags"
fi

if ! ( "${READTAGS}" -h | grep -q -e -Q ); then
    skip "no qualifier function in readtags"
fi

echo ';; (and $end (> $end 14) (< $end 18))' &&
    ${V} ${READTAGS} -e -t output.tags -Q '(and $end (> $end 14) (< $end 18))' -l &&
    :
