# Copyright: 2021 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

is_feature_available ${CTAGS} debug

${CTAGS} --quiet --options=NONE --langdef=foo --map-foo=+.foo -o - input.foo
