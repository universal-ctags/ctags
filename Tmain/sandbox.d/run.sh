#!/bin/sh

# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
is_feature_available $CTAGS sandbox
is_feature_available ${CTAGS} interactive
is_feature_available ${CTAGS} '!' gcov

{
    echo '{"command":"generate-tags", "filename":"input.c", "size": 28}'
    echo 'int main(void) { return 0; }'
} | $CTAGS --quiet --options=NONE  --_interactive=sandbox

exit $?
