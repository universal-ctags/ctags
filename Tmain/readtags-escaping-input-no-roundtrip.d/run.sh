#!/bin/sh

# Copyright: 2022 Masatake YAMATO
# License: GPL-2

READTAGS=$3

. ../utils.sh

if ! [ -x "${READTAGS}" ]; then
	skip "no readtags"
fi

${READTAGS} -eE -t output.tags -l
${READTAGS} -e  -t output.tags -l
