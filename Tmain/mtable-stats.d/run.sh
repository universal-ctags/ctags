# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

${CTAGS} --options=NONE --options=./args.ctags --_mtable-totals=yes -o - ./input.foo
