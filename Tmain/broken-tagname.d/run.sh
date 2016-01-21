#!/bin/sh

# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --options=NONE -o - --language-force=CTagsSelfTest --verbose input.cst

exit $?
