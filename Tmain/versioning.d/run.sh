#!/bin/sh
# Copyright: 2022 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --langdef=TEST'{version=a.9}' --version=TEST
${CTAGS} --quiet --options=NONE --langdef=TEST'{version=10.b}' --version=TEST
${CTAGS} --quiet --options=NONE --langdef=TEST'{version=-1.9}' --version=TEST
${CTAGS} --quiet --options=NONE --langdef=TEST'{version=10.-3}' --version=TEST

${CTAGS} --quiet --options=NONE --options=./test.ctags --version=TEST
${CTAGS} --quiet --options=NONE --options=./test.ctags --list-kinds-full=TEST
${CTAGS} --quiet --options=NONE --options=./test.ctags --list-roles=TEST
${CTAGS} --quiet --options=NONE --options=./test.ctags --list-fields=TEST
${CTAGS} --quiet --options=NONE --options=./test.ctags --list-extras=TEST
${CTAGS} --quiet --options=NONE --options=./test.ctags --describe-language=TEST

${CTAGS} --quiet --options=NONE --options=./broken-kind.ctags --_force-quit=0
echo broken kind: $? 1>&2
${CTAGS} --quiet --options=NONE --options=./broken-role.ctags --_force-quit=0
echo broken role: $? 1>&2
${CTAGS} --quiet --options=NONE --options=./broken-field.ctags --_force-quit=0
echo broken field: $? 1>&2
${CTAGS} --quiet --options=NONE --options=./broken-extra.ctags --_force-quit=0
echo broken extra: $? 1>&2

${CTAGS} --quiet --options=NONE --options=./warning-kind.ctags --_force-quit=0
echo warning kind: $? 1>&2
${CTAGS} --quiet --options=NONE --options=./warning-role.ctags --_force-quit=0
echo warning role: $? 1>&2
${CTAGS} --quiet --options=NONE --options=./warning-field.ctags --_force-quit=0
echo warning field: $? 1>&2
${CTAGS} --quiet --options=NONE --options=./warning-extra.ctags --_force-quit=0
echo warning extra: $? 1>&2
