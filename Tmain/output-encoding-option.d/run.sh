#!/bin/sh
CTAGS=$1

if ${CTAGS} --list-features | grep -q multibyte ; then
  ${CTAGS} --output-encoding=cp932 --input-encoding=utf-8 --input-encoding-javascript=euc-jp input.js input.java
  exit $?
else
  exit 77
fi
