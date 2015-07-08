#!/bin/sh
CTAGS=$1

if ${CTAGS} --quiet --options=NONE --list-features | grep -q multibyte ; then
  ${CTAGS} --quiet --options=NONE --output-encoding=utf-8	input.js input.java
  exit $?
else
  exit 77
fi
