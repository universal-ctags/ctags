#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

BUILDDIR=$2
OPTSCRIPT=$4

. ../utils.sh

if ! [ -x "${OPTSCRIPT}" ]; then
	skip "no optscript"
fi


_VALGRIND_EXIT=58

if type valgrind > /dev/null 2>&1; then
	V()
	{
		vlog=$1
		shift
		valgrind --leak-check=full --error-exitcode=${_VALGRIND_EXIT} --log-file="${vlog}" "$@"
	}
else
	V()
	{
		vlog=$1
		shift
		touch $vlog
		"$@"
	}
fi

rm -f ${BUILDDIR}/*.tmp

for i in $(ls *.ps); do
    printf "%s" "${i}..."
    o=${BUILDDIR}/$(basename $i .ps).out.tmp
    e=$(basename $i .ps).expected
	v=${BUILDDIR}/$(basename $i .ps).valgrind.tmp

	case $i in
		error-*.ps)
			V $v ${OPTSCRIPT} $i > $o 2>&1
			;;
		*)
			V $v ${OPTSCRIPT} $i > $o
			;;
	esac
	s=$?
    printf "%s" "$s"

	if [ "$s" = "$_VALGRIND_EXIT" ]; then
		echo
		cat $v;
		exit $_VALGRIND_EXIT
	fi

	case $s-$i in
		*-error-*.ps|0-*)
			echo
			if diff -ruN --strip-trailing-cr $e $o; then
			   rm $o
			   rm $v
			fi
			;;
		*)
			echo ERROR
			;;
    esac
done
