#!/bin/sh

# Copyright: 2021 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2
OPTSCRIPT=$4

. ../utils.sh

if ! [ -x "${OPTSCRIPT}" ]; then
	skip "no optscript"
fi

rm -f ${BUILDDIR}/*.tmp

for t in $(ls *.ps); do
	i=${BUILDDIR}/${t}.in.tmp
	o=${BUILDDIR}/${t}.out.tmp
	e=$(basename $t .ps).expected

    printf "%s" "${t}..."

	{
		${CTAGS} --_dump-prelude
		echo
		cat $t
	} > $i

	${OPTSCRIPT} $i > $o 2>&1
	s=$?
	echo "$s"

	if [ $s != 0 ]; then
		continue
	fi
	if diff -ruN --strip-trailing-cr $e $o; then
		rm $i
		rm $o
	fi
done
