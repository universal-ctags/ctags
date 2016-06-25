#!/bin/sh
# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE			\
	 --print-language			\
	 --langdef=AAA --map-AAA='(input.xxx)'	\
	 --langdef=BBB --map-BBB=.xxx      	\
	 input.xxx &&
${CTAGS} --quiet --options=NONE			\
	 --print-language			\
	 --langdef=AAA --map-AAA=.xxx      	\
	 --langdef=BBB --map-BBB='(input.xxx)'	\
	 input.xxx

exit $?

