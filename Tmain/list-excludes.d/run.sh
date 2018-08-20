# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --exclude='foo' --list-excludes
