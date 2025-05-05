#!/bin/sh

# Copyright: 2025 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

is_feature_available "$CTAGS" debug
${CTAGS} --quiet --options=NONE --langdef=BROKEN'{version=0.1}' --_force-quit=1
exit $?
