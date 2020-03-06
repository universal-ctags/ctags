# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1
O="--quiet --options=NONE "

${CTAGS} $O \
		 --extras=+p --pseudo-tags=TAG_KIND_DESCRIPTION \
		 --kinds-C=df \
		 -o - \
		 input.c
