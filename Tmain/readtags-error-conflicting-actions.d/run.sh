#!/bin/sh

# Copyright: 2024 Masatake YAMATO
# License: GPL-2

BUILDDIR=$2
READTAGS=$3
#V="valgrind --leak-check=full --track-origins=yes -v"
V=

. ../utils.sh

skip_if_no_readtags "$READTAGS"

f=${BUILDDIR}/tmp-readtags-error-conflicting-actions-$$
rm -f "$f"

{
	${READTAGS} --tag-file input.tags -l - main
} 2> "$f"

s=$?

sed 's|.*\(readtags[^:]*\):|readtags:|' < "$f" 1>&2
rm "$f"

exit $s
