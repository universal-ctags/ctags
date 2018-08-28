# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

echo '#' warning messages 1>&2
(
	${CTAGS} --options=NONE --options=./args.ctags -o - ./input0.foo
)

echo '#' fatal regex message 1>&2
(
	${CTAGS} --options=NONE --options=./args.ctags -o - ./input1.foo
)

echo '#' fatal mline-regex message 1>&2
(
	${CTAGS} --options=NONE --options=./args.ctags -o - ./input2.foo
)

echo '#' fatal mtable-regex message 1>&2
(
	${CTAGS} --options=NONE --options=./args.ctags -o - ./input3.foo
)
