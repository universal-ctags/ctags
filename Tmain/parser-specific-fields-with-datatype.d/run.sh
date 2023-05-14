# Copyright: 2025 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --options=testlang.ctags -o - \
		 --output-format=u-ctags \
		 input.testlang
