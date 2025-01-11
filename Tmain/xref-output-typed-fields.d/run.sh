# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo '# input.ctst'
${CTAGS} --quiet --options=NONE -o - \
		 --output-format=xref --_xformat='%{name} -> b=%{CTagsSelfTest.bField},sb=%{CTagsSelfTest.sbField},s=%{CTagsSelfTest.sField}' \
		 --language-force=CTagsSelfTest input.ctst

echo '# input.rst'
${CTAGS} --quiet --options=NONE -o - \
		 --output-format=xref --_xformat='%{name} -> %{RestructuredText.overline}' \
		 input.rst

echo '# input.c'
${CTAGS} --quiet --options=NONE -o - \
		 --output-format=xref --_xformat='%{name} -> %{file}' \
		 input.c
