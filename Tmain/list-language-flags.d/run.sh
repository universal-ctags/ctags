#!/bin/sh
# Copyright: 2025 Masatake YAMATO
# License: GPL-2

CTAGS=$1

: &&
	echo langdef-flags: &&
	${CTAGS} --quiet --options=NONE --_list-langdef-flags &&
	echo fielddef-flags: &&
	${CTAGS} --quiet --options=NONE --_list-fielddef-flags &&
	echo kinddef-flags: &&
	${CTAGS} --quiet --options=NONE --_list-kinddef-flags &&
	echo roledef-flags: &&
	${CTAGS} --quiet --options=NONE --_list-roledef-flags &&
	echo extradef-flags: &&
	${CTAGS} --quiet --options=NONE --_list-extradef-flags &&
	:
