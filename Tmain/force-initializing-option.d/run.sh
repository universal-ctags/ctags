#!/bin/sh
# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --_fatal-warnings --_force-initializing --_force-quit=0

exit $?
