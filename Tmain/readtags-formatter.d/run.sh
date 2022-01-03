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

if ! ( "${READTAGS}" -h | grep -q -e -F ); then
    skip "no formatter function in readtags"
fi

echo '# FQ' &&
	${V} ${READTAGS} -t output.tags -Q '(eq? $kind "function")' -F '(list $name #t)' -l &&
echo '# F' &&
${V} ${READTAGS} -t output.tags -F '(if (eq? $kind "function") (list $name #t) #f)' -l &&
echo '# F declarations' &&
${V} ${READTAGS} -t output.tags -F \
     '(if (eq? $kind "function")
         (list (if $file "static " #f) $typeref-name " " $name $signature ";" #t)
        #f)' -l
