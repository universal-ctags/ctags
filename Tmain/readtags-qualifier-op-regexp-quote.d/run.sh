#!/bin/sh

# Copyright: 2021 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

${V} ${READTAGS} -e -t output.tags \
	 -Q '(if (prefix? $name "a") #f ((string->regexp (string-append (regexp-quote $name) ".?")) $name))' \
	 -l

${V} ${READTAGS} -e -t output.tags \
	 -Q '(if (prefix? $name "a") #f ((string->regexp (string-append (regexp-quote "[{.*+]}^$()|?\\") ".?")) $name))' \
	 -l
