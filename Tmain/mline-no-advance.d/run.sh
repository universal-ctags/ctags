# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1
${CTAGS} --options=NONE \
		 --langdef=FOO  \
		 --map-FOO=+.foo \
		 --options=./args.ctags \
		 -o - \
		 ./input.foo
