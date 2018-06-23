#!/bin/sh

# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
is_feature_available $CTAGS debug

# For comparison the output and the expectation, sed removes signature parts from the output.
$CTAGS --_trace=CTagsSelfTest --language-force=CTagsSelfTest ./input.unknown
exit $?
