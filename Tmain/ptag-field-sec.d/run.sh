# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1
O="--quiet --options=NONE "

${CTAGS} $O \
		 --extras=+p --pseudo-tags=TAG_FIELD_DESCRIPTION \
		 --fields=le \
		 --fields-C='{macrodef}' \
		 -o - \
		 input.c
