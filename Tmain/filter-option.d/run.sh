#!/bin/sh
# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo ./input.c | $CTAGS --filter
echo ./input.c | $CTAGS --filter --output-format=xref
