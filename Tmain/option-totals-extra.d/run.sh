# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --totals=extra --language-force=CTagsSelfTest -o - input.unknown 2>&1 \
	| grep -v ^N \
	| sed -ne '/^STATISTICS.*/,$p'
