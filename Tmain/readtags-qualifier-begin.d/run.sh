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

run_readtags()
{
	local fexpr=$1
	local action=$2
	echo ';;' "$fexpr" &&
		echo ';;' "$fexpr" 1>&2 &&
		${V} ${READTAGS} -e -t output.tags -Q "$fexpr" "$action"
}

run_readtags '(begin #f (print $name) (print $line) #t)' -l &&
run_readtags '(begin #t (print $name) (print $line) #f)' -l &&
run_readtags '(begin #t (print $name) (print $line) (eq? $name "efg"))' -l &&
run_readtags '(begin #t (and (eq? $name "abc") (print $name) (print $line)) (eq? $name "efg"))' -l &&
run_readtags '(begin0 #t                (print $name) (print $line) #f)' -l &&
run_readtags '(begin0 #f                (print $name) (print $line) #t)' -l &&
run_readtags '(begin0 (eq? $name "abc") (print $name) (print $line) #t)' -l
run_readtags '(begin0 (eq? $name "abc") (and (eq? $name "efg") (print $name) (print $line)) #t)' -l
