#!/bin/sh
# Copyright: 2022 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --langdef=TEST'{version=10.9}' --version=TEST
