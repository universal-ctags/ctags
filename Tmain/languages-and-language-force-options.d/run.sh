# Copyright: 2016 Masatake YAMATO
# License: GPL-2
CTAGS=$1

${CTAGS} --quiet --options=NONE --language-force=C --languages=-C -o - input.c
