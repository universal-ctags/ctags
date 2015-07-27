#!/bin/sh
CTAGS=$1

if ${CTAGS} --quiet --options=NONE --list-features | grep -q multibyte ; then
  ${CTAGS} --quiet --options=NONE --input-encoding-java=invalid	--input-encoding-javascript=euc-jp input.js input.java
  exit $?
else
  echo "multibyte feature is not available"
  exit 77
fi
