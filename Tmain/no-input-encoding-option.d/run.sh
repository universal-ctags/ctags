#!/bin/sh
# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

. ../utils.sh

if ${CTAGS} --quiet --options=NONE --list-features | grep -q iconv; then
  ${CTAGS} --quiet --options=NONE --output-encoding=utf-8 -o $BUILDDIR/tags input.js input.java
  exit $?
else
  skip "iconv feature is not available"
fi
