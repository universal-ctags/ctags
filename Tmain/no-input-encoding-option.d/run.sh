#!/bin/sh
# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

if ${CTAGS} --quiet --options=NONE --list-features | grep -q multibyte ; then
  ${CTAGS} --quiet --options=NONE --output-encoding=utf-8	input.js input.java
  exit $?
else
  echo "multibyte feature is not available"
  exit 77
fi
