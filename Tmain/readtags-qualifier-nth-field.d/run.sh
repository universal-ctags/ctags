#!/bin/sh

# Copyright: 2016 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

${V} ${READTAGS} -e -t output.tags \
	 -Q '(eq? $kind "z")' \
	 -S '(<> $nth &nth)' \
	 -F '(list $name ":" $nth #t)' \
	 -l
