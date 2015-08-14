#!/bin/sh
CTAGS=$1
BUILDDIR=$4

. ../utils.sh

if ${CTAGS} --quiet --options=NONE --list-features | grep -q multibyte ; then
  if ${CTAGS}  --quiet --options=NONE \
	       --output-encoding=cp932 --input-encoding=utf-8 --input-encoding-javascript=euc-jp \
	       -o ${BUILDDIR}/tags \
	       input.js input.java ; then
      remove_commit_id ${BUILDDIR}/tags
  fi
  exit $?
else
  echo "multibyte feature is not available"
  exit 77
fi
