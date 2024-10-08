#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

READTAGS=$3
#V="valgrind --leak-check=full --track-origins=yes -v"
V=

. ../utils.sh

skip_if_no_readtags "$READTAGS"

echo '# SORT=NO'
${V} ${READTAGS} -t ./ptag-sort-no.tags -D

echo '# SORT=YES'
${V} ${READTAGS} -t ./ptag-sort-yes.tags -D
