#!/bin/sh

# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILD_SUBDIR=$3
stderr=${BUILD_SUBDIR}/stderr-actual.txt

${CTAGS} --options=NONE -o - --language-force=CTagsSelfTest --verbose input.cst

# externalSortTags invokes sort command, and it is logged to stderr.
# Delete the line for the comparison.
if [ -w "${stderr}" ]; then
   sed -i -e '/^system ("sort -u")$/d' "${stderr}"
fi

exit $?
