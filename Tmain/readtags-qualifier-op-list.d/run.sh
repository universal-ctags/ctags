#!/bin/sh

# Copyright: 2026 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

qexpr='(not (member $kind (list "m" "z" "p" "f" "s")))'
echo "-Q '${qexpr}'"
${V} ${READTAGS} -et output.tags \
	 -Q "$qexpr" \
	 -l
echo $?

qexpr='(member $kind (list "z" "p"))'
echo "-Q '${qexpr}'"
${V} ${READTAGS} -et output.tags \
	 -Q "$qexpr" \
	 -l
echo $?

sexpr='(<or> (if (and (member $kind (list "s" "v")) (member &kind (list "s" "v"))) (<> $name &name) 0) (<> &name $name))'
echo "-S $sexpr"
${V} ${READTAGS} -et output.tags \
	 -S "$sexpr" \
	 -l
echo $?
