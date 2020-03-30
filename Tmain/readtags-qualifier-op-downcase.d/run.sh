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

if ! ( "${READTAGS}" -h | grep -q -e -Q ); then
    skip "no qualifier function in readtags"
fi

echo '# (downcase $name)'
${V} ${READTAGS} -t output.tags -Q '(eq? "c" (downcase $name))' -l

echo '# (member "a" (downcase $inherits))'
${V} ${READTAGS} -t output.tags -Q '(member "a" (downcase $inherits))' -l

echo '# (member "b" (downcase $inherits))'
${V} ${READTAGS} -t output.tags -Q '(member "b" (downcase $inherits))' -l

echo '# (member "d" (downcase $inherits))'
${V} ${READTAGS} -t output.tags -Q '(member "c" (downcase $inherits))' -l
