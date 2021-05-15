# Copyright: 2021 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
: &&
	${CTAGS} --quiet --options=NONE\
			 --_prelude-C='{{ (enter C) == }}' \
			 --_sequel-C='{{ (leave C) == }}' \
			 --options=./args-c.ctags -o - input.c &&
	${CTAGS} --quiet --options=NONE \
			 --_prelude-DTS='{{ (enter DTS) == }}' \
			 --_sequel-DTS='{{ (leave DTS) == }}' \
			 --options=./args-dts.ctags -o - input.dts &&
	${CTAGS} --quiet --options=NONE --map-CPreProcessor=+.i \
			 --_prelude-CPreProcessor='{{ (enter CPreProcessor) == }}' \
			 --_sequel-CPreProcessor='{{ (leave CPreProcessor) == }}' \
			 --options=./args-cpreprocessor.ctags -o - input.i

#
# BUGS: The following two don't work because in-use marker is not set to
# the subparsers of the CPreprocessor parser. The CPreprocessor parser must
# mark in-use on its sub parsers to call the inputStart and inputEnd methods
# in the foreachSubparser loop.
#
#	${CTAGS} --quiet --options=NONE --options=./args-cpreprocessor.ctags -o - input.c
#	${CTAGS} --quiet --options=NONE --options=./args-cpreprocessor.ctags -o - input.dts
#
