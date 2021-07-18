#!/bin/sh

# Copyright: 2021 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

${CTAGS} --quiet --options=NONE --options=./args.ctags -o - input.unknown
