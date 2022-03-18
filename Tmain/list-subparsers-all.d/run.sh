# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1"

. ../utils.sh
is_feature_available $CTAGS xpath
is_feature_available $CTAGS yaml

${CTAGS} --quiet --options=NONE \
		 --list-subparsers=all
