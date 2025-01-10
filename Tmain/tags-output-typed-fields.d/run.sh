# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE -o - \
		 --output-format=u-ctags \
		 --language-force=CTagsSelfTest input.ctst
