# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1"

. ../utils.sh
is_feature_available $CTAGS xpath

${CTAGS} --quiet --options=NONE \
		 --list-subparsers=all \
	| grep -v AnsiblePlaybook
