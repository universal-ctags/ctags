#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

READTAGS=$3
#V="valgrind --leak-check=full --track-origins=yes -v"
V=

. ../utils.sh

skip_if_no_readtags "$READTAGS"

echo '# SORT=NO (-D)'
${V} ${READTAGS} -t ./ptag-sort-no.tags -D

echo '# SORT=NO (-P)'
${V} ${READTAGS} -t ./ptag-sort-no.tags -P no-such-tags

echo '# SORT=YES (-D)'
${V} ${READTAGS} -t ./ptag-sort-yes.tags -D

echo '# SORT=YES (-P)'
${V} ${READTAGS} -t ./ptag-sort-yes.tags -P no-such-tags

# Without the following environment variable,
# Msys2 convert #/MA/i to #C:/msys64/MA/i.
# See https://www.msys2.org/docs/filesystem-paths/
export MSYS2_ARG_CONV_EXCL='*'

echo '# -D + FILTER (-D)'
${V} ${READTAGS} -t ./ptag-sort-yes.tags -Q '(#/MA/i $name)' -D

echo '# -P -l + FILTER (-D)'
${V} ${READTAGS} -t ./ptag-sort-yes.tags -Q '(#/MA/i $name)' -P -l
