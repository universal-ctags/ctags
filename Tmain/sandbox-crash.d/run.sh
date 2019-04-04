#!/bin/sh

# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
is_feature_available $CTAGS debug
is_feature_available $CTAGS sandbox
is_feature_available ${CTAGS} interactive
is_feature_available ${CTAGS} '!' gcov

{
    echo '{"command":"generate-tags", "filename":"input.ctst", "size": 1}'
    echo 'P'
} | $CTAGS --quiet --options=NONE  --language-force=CTagsSelfTest --_interactive=sandbox

exit $?
