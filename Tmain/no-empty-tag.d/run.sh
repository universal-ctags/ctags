#!/bin/sh

# Copyright: 2024 Masatake YAMATO
# License: GPL-2

CTAGS=$1

$CTAGS --options=NONE -o - input-0.mak > /dev/null
