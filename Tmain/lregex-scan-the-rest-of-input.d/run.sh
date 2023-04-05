#!/bin/sh

# Copyright: 2023 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE -o - \
		 --kinddef-CTagsSelfTest=a,name,names \
		 --regex-CTagsSelfTest='/restdef:([a-z])/\1/a/' \
		 --language-force=CTagsSelfTest input.cst

exit $?
