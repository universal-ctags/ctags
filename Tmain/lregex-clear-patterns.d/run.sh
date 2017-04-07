# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo '#' Add a pattern
${CTAGS} --quiet --options=NONE \
		 --options=./mylang.ctags \
		 -o - \
		 input.c

echo '#' Clear the pattern
${CTAGS} --quiet --options=NONE \
		 --options=./mylang.ctags \
		 --regex-MYLANG= \
		 -o - \
		 input.c
