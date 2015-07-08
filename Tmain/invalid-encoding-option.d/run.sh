#!/bin/sh
CTAGS=$1

if ${CTAGS} --list-features | grep -q multibyte ; then
  ${CTAGS} --input-encoding-java=invalid	--input-encoding-javascript=euc-jp input.js input.java
  exit $?
else
  exit 77
fi
