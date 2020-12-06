# Copyright: 2016 Aman Gupta
# License: GPL-2

CTAGS=$1

. ../utils.sh

if is_feature_available "${CTAGS}" json; then
    {
	run_with_format json --languages=+man --fields=-T
	run_with_format json --languages=+man --fields="*"-T
	run_with_format json --languages=+man --fields="*"-T --extras='*'
    } | cat \
		| grep -v TAG_PROGRAM_VERSION \
		| grep -v TAG_OUTPUT_FILESEP  \
		| grep -v TAG_PROC_CWD
fi
