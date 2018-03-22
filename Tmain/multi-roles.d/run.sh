#!/bin/sh
# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1
echo '# rolesDisabled is kept disabled'
$CTAGS --quiet --options=NONE  --sort=no --language-force=CTagsSelfTest --extras=r --fields=Kr -o - input.x
echo '# rolesDisabled is enabled'
$CTAGS --quiet --options=NONE  --sort=no --language-force=CTagsSelfTest --extras=r --fields=Kr --kinds-CTagsSelfTest=+R -o - input.x

exit $?
