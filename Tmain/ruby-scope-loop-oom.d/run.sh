#!/bin/sh

# Copyright: 2022 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

if ! type timeout > /dev/null 2>&1; then
	skip "timeout command is not available"
fi

timeout 5s ${CTAGS} --quiet --options=NONE --fields=+e -o - a.rb b.rb
exit $?
