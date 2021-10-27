# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

is_feature_available ${CTAGS} pcre2

${CTAGS} --quiet --options=NONE --list-regex-flags
