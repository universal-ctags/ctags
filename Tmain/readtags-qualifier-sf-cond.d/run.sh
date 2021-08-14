#!/bin/sh

# Copyright: 2021 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
    skip "no readtags"
fi

if ! ( "${READTAGS}" -h | grep -q -e -S ); then
    skip "no qualifier function in readtags"
fi

${V} ${READTAGS} -e -t output.tags \
	 -Q '(cond ((eq? $kind "m") (#/.h./ $name)) ((eq? $kind "t")) ((#/.n./ $name) #f) (#t))' \
	 -l
