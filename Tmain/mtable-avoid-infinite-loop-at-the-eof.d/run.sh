# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1
${CTAGS} --options=NONE --options=./args.ctags --sort=no -o - ./input.foo ./input2.foo
