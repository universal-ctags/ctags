#!/bin/sh

# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
is_feature_available $CTAGS sandbox
is_feature_available ${CTAGS} interactive
is_feature_available ${CTAGS} '!' gcov

{
    echo '{"command":"generate-tags", "filename":"input.unknown", "size": 44}'
    echo '/* -*- c -*- */ int main(void) { return 0; }'
} | $CTAGS --quiet --options=NONE  -G --_interactive=sandbox

exit $?
