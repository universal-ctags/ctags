#!/bin/sh

# Copyright: 2020 Masatake YAMATO
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

echo '# (<> $name &name)'
${V} ${READTAGS} -t output.tags -S '(<> $name &name)' -p hi

echo '# (<or> (<> (length $name) (length &name)) (<> $name &name))'
${V} ${READTAGS} -t output.tags -S '(<or> (<> (length $name) (length &name)) (<> $name &name))' -p hi
