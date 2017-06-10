#!/bin/sh

# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILD_SUBDIR=$2
stderr_tmp=${BUILD_SUBDIR}/stderr-actual.txt.tmp

${CTAGS} --options=NONE -o - --language-force=CTagsSelfTest --verbose input.cst \
	 2> ${stderr_tmp}

# externalSortTags invokes sort command, and it is logged to stderr.
# Delete the line for the comparison.
sed -e '/^system ("sort -u")$/d' < ${stderr_tmp} 1>&2
rm ${stderr_tmp}

exit $?
