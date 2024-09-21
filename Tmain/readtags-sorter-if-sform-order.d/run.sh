#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
	skip "no readtags"
fi

${READTAGS} -t output.tags -ne -Q '(if $extras (print $extras) #f)' -l
