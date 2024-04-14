#!/bin/sh

# Copyright: 2024 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

#V="valgrind --leak-check=full -v"
V=

if ! [ -x "${READTAGS}" ]; then
	skip "no readtags"
fi

if ! ( "${READTAGS}" -h | grep -q -e -Q ); then
    skip "no qualifier function in readtags"
fi

# ?a
# 97
# (format "%c" 96)
# => `
"${READTAGS}" -t output.tags -S '(<> ($ "properties" "`") (& "properties" "`"))' -F '(list $name " " ($ "properties" "noprop") #t)' -l
