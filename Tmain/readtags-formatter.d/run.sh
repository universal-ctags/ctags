#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

echo '# FQ' &&
	${V} ${READTAGS} -t output.tags -Q '(eq? $kind "function")' --formatter '(list $name #t)' -l &&
echo '# F' &&
${V} ${READTAGS} -t output.tags -F '(if (eq? $kind "function") (list $name #t) #f)' -l &&
echo '# F declarations' &&
${V} ${READTAGS} -t output.tags -F \
     '(if (eq? $kind "function")
         (list (if $file "static " #f) $typeref-name " " $name $signature ";" #t)
        #f)' -l
