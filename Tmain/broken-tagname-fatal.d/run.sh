#!/bin/sh

# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

ulimit -c 0
${CTAGS} --options=NONE --_fatal-warnings  -o - --language-force=CTagsSelfTest input.cst

exit $?
