#!/bin/sh
# Copyright: 2022 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --options=./test.ctags --version=TEST
${CTAGS} --quiet --options=NONE --options=./test.ctags --list-kinds-full=TEST
${CTAGS} --quiet --options=NONE --options=./test.ctags --list-roles=TEST
${CTAGS} --quiet --options=NONE --options=./test.ctags --list-fields=TEST
${CTAGS} --quiet --options=NONE --options=./test.ctags --list-extras=TEST
${CTAGS} --quiet --options=NONE --options=./test.ctags --describe-language=TEST
