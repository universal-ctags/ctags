#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

echo '# (<> $name &name)'
${V} ${READTAGS} -t output.tags -S '(<> $name &name)' -p hi

echo '# (<or> (<> (length $name) (length &name)) (<> $name &name))'
${V} ${READTAGS} -t output.tags -S '(<or> (<> (length $name) (length &name)) (<> $name &name))' -p hi
