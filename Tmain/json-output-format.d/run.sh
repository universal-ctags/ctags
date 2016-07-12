# Copyright: 2016 Aman Gupta
# License: GPL-2

CTAGS=$1

. ../utils.sh

if is_feature_available ${CTAGS} json; then
    {
	run_with_format json
	run_with_format json --fields="*"
	run_with_format json --fields="*" --extra='*'
    } | grep -v TAG_PROGRAM_VERSION
fi
