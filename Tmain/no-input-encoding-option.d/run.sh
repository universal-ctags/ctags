#!/bin/sh
CTAGS=$1

if ${CTAGS} --list-features | grep -q multibyte ; then
  ${CTAGS} --output-encoding=utf-8	input.js input.java
  exit $?
else
  exit 77
fi
