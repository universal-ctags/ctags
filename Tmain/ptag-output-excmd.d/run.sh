# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

O="--quiet --options=NONE "

for c in number pattern mixed combine; do
	${CTAGS} $O \
			 --excmd=$c \
			 --extras=+p --pseudo-tags=TAG_OUTPUT_EXCMD \
			 -o - \
			 input.c
done
