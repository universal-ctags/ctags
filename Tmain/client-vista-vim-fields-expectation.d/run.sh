# Copyright: 2021 <https://github.com/liuchengxu>
# License: GPL-2

CTAGS=$1

. ../utils.sh

is_feature_available "${CTAGS}" json

$CTAGS --quiet --options=NONE --format=2 --excmd=pattern --fields=+nksSaf --extras=+F --sort=no --append=no --extras=  --language-force=vim --vim-kinds=acfvmn --output-format=json --fields=-PF -f- test.vim

exit $?
