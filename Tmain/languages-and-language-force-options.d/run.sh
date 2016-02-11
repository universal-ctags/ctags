# Copyright: 2016 Masatake YAMATO
# License: GPL-2
CTAGS=$1

${CTAGS} --language-force=C --languages=-C input.c
