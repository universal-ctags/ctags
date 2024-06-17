#!/bin/sh

# Copyright: 2024 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
	skip "no readtags"
fi

if ! ( "${READTAGS}" -h | grep -q -e -S ); then
	skip "no sorter function in readtags"
fi

ERRLOG=/tmp/ctags-Tmain-$$
${READTAGS} -et output.tags -S '
(<or> (<> $name &name)
      (and (eq? $kind "p") (eq? &kind "f")  1)
      (and (eq? $kind "f") (eq? &kind "p") -1))' -l
