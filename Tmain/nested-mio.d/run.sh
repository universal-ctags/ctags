# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1
${CTAGS} --options=NONE -o - --extras=+g --language-force=CTagsSelfTest input.cst
exit $?
