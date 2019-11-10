#!/bin/sh

# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1

if ! $CTAGS --quiet --options=NONE --kinds-all=xyz --_force-quit=0; then
	$CTAGS --quiet --options=NONE --all-kinds=abc --_force-quit=0
else
	exit 0
fi
