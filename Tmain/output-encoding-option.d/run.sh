#!/bin/sh
# Copyright: 2015 Yasuhiro MATSUMOTO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

. ../utils.sh

if ${CTAGS} --quiet --options=NONE --list-features | grep -q iconv; then
  check_encoding shift_jis
  check_encoding utf-8
  check_encoding euc-jp
  if ${CTAGS}  --quiet --options=NONE \
	       --pseudo-tags=-TAG_PROC_CWD \
	       --pseudo-tags=-TAG_PROGRAM_VERSION \
	       --pseudo-tags=-TAG_KIND_DESCRIPTION \
	       --pseudo-tags=-TAG_FIELD_DESCRIPTION \
	       --pseudo-tags=-TAG_EXTRA_DESCRIPTION \
	       --pseudo-tags=-TAG_ROLE_DESCRIPTION \
	       --pseudo-tags=-TAG_PARSER_VERSION \
	       --pseudo-tags=-TAG_OUTPUT_VERSION \
	       --output-encoding=shift_jis --input-encoding=utf-8 --input-encoding-javascript=euc-jp \
	       -o ${BUILDDIR}/tags \
	       input.js input.java ; then
      remove_commit_id ${BUILDDIR}/tags
  fi
  exit $?
else
	skip "iconv feature is not available"
fi
