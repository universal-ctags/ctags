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

if ! ( "${READTAGS}" -h | grep -q -e -Q ); then
    skip "no qualifier function in readtags"
fi


"${READTAGS}" -t output.tags -en -D -l \
	| "${READTAGS}" -t - -en -Q '(eq? $signature "()")' -D -l \
	| "${READTAGS}" -t - -en -Q '(and $line (< $line 10))' -D -l \
	| "${READTAGS}" -t - -en -Q '(substr? $name ".a")' -D -l \
	| "${READTAGS}" -t - -en -S '(<> $line &line)' -D -l
