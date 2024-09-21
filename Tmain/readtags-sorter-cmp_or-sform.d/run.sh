#!/bin/sh

# Copyright: 2024 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

ERRLOG=/tmp/ctags-Tmain-$$
${READTAGS} -et output.tags -S '
(<or> (<> $name &name)
      (and (eq? $kind "p") (eq? &kind "f")  1)
      (and (eq? $kind "f") (eq? &kind "p") -1))' -l
