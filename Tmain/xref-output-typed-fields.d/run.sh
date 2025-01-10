# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE -o - \
		 --output-format=xref --_xformat='%{name} -> b=%{CTagsSelfTest.bField},sb=%{CTagsSelfTest.sbField}' \
		 --language-force=CTagsSelfTest input.ctst
