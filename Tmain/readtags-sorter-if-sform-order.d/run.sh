#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

skip_if_no_readtags "$READTAGS"

${READTAGS} -t output.tags -ne -Q '(if $extras (print $extras) #f)' -l
