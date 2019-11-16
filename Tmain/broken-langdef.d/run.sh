#!/bin/sh

# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1

run_ctags()
{
	echo '#' "$1" 1>&2
	${CTAGS} --quiet --options=NONE --langdef="$1" --kinddef-"$1"=f,func,functions --_force-quit=0 -o - input.c
}

run_ctags ""
run_ctags all
for c in '!' '"' '$' '%' '&' "'" '(' ')' '*' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '@' '[' '\' ']' '^' '`' '|' '~'; do
	run_ctags "C$c"
done
