# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1"

${CTAGS} --quiet --options=NONE \
		 --options=./event.ctags \
		 --options=./hook.ctags \
		 --fields=+lK \
		 -o - input.c
