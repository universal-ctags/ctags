#!/bin/sh

# Copyright: 2016 Masatake YAMATO
# License: GPL-2

READTAGS=$4

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
    echo "no readtags"
    eixt 77
fi

if ! ( "${READTAGS}" -h | grep -q -e -Q ); then
    echo "no qualifier function in readtags"
    exit 77
fi

echo ';; (and $end (> $end 14) (< $end 18))' &&
    ${V} ${READTAGS} -e -t output.tags -Q '(and $end (> $end 14) (< $end 18))' -l &&
    :
