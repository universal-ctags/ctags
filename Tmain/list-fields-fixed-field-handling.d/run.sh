# Copyright: 2019 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS="$1"

is_feature_available "${CTAGS}" json

run()
{
	echo '#' "$@"
	$CTAGS --quiet --options=NONE "$@" | grep -v -e ^-
}

run --output-format=u-ctags  --fields=n --list-fields
run --fields=n --output-format=u-ctags  --list-fields
run --output-format=json  --fields=n --list-fields
run --fields=n --output-format=json  --list-fields
