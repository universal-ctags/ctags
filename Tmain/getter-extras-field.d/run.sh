# Copyright: 2021 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --verbose --options=NONE -o - --options=x.ctags input.unknown
