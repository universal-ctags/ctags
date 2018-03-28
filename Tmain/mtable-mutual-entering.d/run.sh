# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --options=NONE --options=./args.ctags -o - \
		 ./input0.foo ./input1.foo \
		 ./input2.bar \
		 ./input3.baz
