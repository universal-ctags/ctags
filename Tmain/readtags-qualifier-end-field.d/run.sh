#!/bin/sh

# Copyright: 2016 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

echo ';; (and $end (> $end 14) (< $end 18))' &&
    ${V} ${READTAGS} -e -t output.tags -Q '(and $end (> $end 14) (< $end 18))' -l &&
    :
