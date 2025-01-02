# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE -o - \
		 --output-format=u-ctags \
		 --language-force=CTagsSelfTest input.ctst

${CTAGS} --quiet --options=NONE -o - \
		 --output-format=u-ctags \
		 --fields-RestructuredText=+'{overline}' \
		 input.rst

${CTAGS} --quiet --options=NONE -o - \
		 --output-format=u-ctags \
		 input.c
