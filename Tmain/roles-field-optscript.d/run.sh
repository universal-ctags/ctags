# Copyright: 2021 Masatake YAMATO
# License: GPL-2
CTAGS=$1

${CTAGS} --quiet --options=foo.ctags -o - input.foo | grep -v '^a'
