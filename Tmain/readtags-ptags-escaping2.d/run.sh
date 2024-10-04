#!/bin/sh

# Copyright: 2022 Masatake YAMATO
# License: GPL-2

READTAGS=$3
#V="valgrind --leak-check=full --track-origins=yes -v"
V=

. ../utils.sh

skip_if_no_readtags "$READTAGS"

for m in e u _; do
    for s in s b _; do
	echo readtags -e -t example-$m-$s.tags -E -D:
	${V} ${READTAGS} -e -t example-$m-$s.tags -E -D
	echo readtags -e -t example-$m-$s.tags -D:
	${V} ${READTAGS} -e -t example-$m-$s.tags -D
	echo
    done
done
