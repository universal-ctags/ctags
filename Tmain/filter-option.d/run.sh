#!/bin/sh
# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo ./input.c | $CTAGS --quiet --options=NONE --filter
echo ./input.c | $CTAGS --quiet --options=NONE --filter --output-format=xref
