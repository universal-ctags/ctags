#!/bin/sh
# Copyright: 2022 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --langdef=TEST'{version=a.9}' --version=TEST
${CTAGS} --quiet --options=NONE --langdef=TEST'{version=10.b}' --version=TEST
${CTAGS} --quiet --options=NONE --langdef=TEST'{version=-1.9}' --version=TEST
${CTAGS} --quiet --options=NONE --langdef=TEST'{version=10.-3}' --version=TEST

${CTAGS} --quiet --options=NONE --langdef=TEST'{version=10.9}' --version=TEST
