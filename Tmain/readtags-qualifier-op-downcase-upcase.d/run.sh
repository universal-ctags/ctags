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

echo '# (eq? "c" (downcase $name))'
${V} ${READTAGS} -t output.tags -Q '(eq? "c" (downcase $name))' -l

echo '# (and $inherits (#/(^|,)a(,|$)/ (downcase $inherits)))'
${V} ${READTAGS} -t output.tags -Q '(and $inherits (#/(^|,)a(,|$)/ (downcase $inherits)))' -l

echo '# (and $inherits (#/(^|,)b(,|$)/ (downcase $inherits)))'
${V} ${READTAGS} -t output.tags -Q '(and $inherits (#/(^|,)b(,|$)/ (downcase $inherits)))' -l

echo '# (and $inherits (#/(^|,)d(,|$)/ (downcase $inherits)))'
${V} ${READTAGS} -t output.tags -Q '(and $inherits (#/(^|,)d(,|$)/ (downcase $inherits)))' -l

echo '# (eq? "F" (upcase $name))'
${V} ${READTAGS} -t output.tags -Q '(eq? "F" (upcase $name))' -l

echo '# (eq? "G" (upcase $name))'
${V} ${READTAGS} -t output.tags -Q '(eq? "G" (upcase $name))' -l

echo '# (eq? "J" (upcase $name))'
${V} ${READTAGS} -t output.tags -Q '(eq? "J" (upcase $name))' -l
