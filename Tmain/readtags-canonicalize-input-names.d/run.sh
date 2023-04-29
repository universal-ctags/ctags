#!/bin/sh

# Copyright: 2021 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
	skip "no readtags"
fi


echo2()
{
	echo "$@"
	echo "$@" 1>&2
}

run_test()
{
	echo2 "# $@"
	"${READTAGS}" -F '(list $name "\t" $input "\n")' $2 -t $1 $3
}

run_test good0.tags -C -l &&
run_test good1.tags --canonicalize-input -l &&
run_test good2.tags -C -l &&
! run_test drive-letter0.tags --canonicalize-input -l &&
! run_test drive-letter1.tags -C -l &&
run_test good-ptags.tags -C -D &&
run_test good0.tags -A -l &&
run_test good1.tags --absolute-input -l &&
run_test good2.tags -A -l &&
! run_test drive-letter0.tags --absolute-input -l &&
! run_test drive-letter1.tags -A -l &&
run_test good-ptags.tags -A -D
