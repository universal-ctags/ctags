# Copyright: 2021 <liuchengxu@github.com>
# License: GPL-2
#!/usr/bin/env bash

CTAGS=$1

. ../utils.sh

$CTAGS --format=2 --excmd=pattern --fields=+nksSaf --extras=+F --sort=no --append=no --extras=  --language-force=vim --vim-kinds=acfvmn --output-format=json --fields=-PF -f- $(pwd)/test.vim

exit $?
