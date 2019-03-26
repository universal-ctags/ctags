# Copyright: 2019 Masatake YAMATO
# License: GPL-2

#
# This is crash test.
# The input is broken so the we have no expectation for output.
#

CTAGS=$1
. ../utils.sh

is_feature_available ${CTAGS} json

$CTAGS --options=NONE --output-format=json input.h
