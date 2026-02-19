# Copyright: 2026 Masatake YAMATO
# License: GPL-2

CTAGS="$1"

. ../utils.sh

if ! type ulimit > /dev/null; then
	skip "ulimit command is not available"
fi

skip_if_runnint_on_qemu_user_mode
is_feature_available "$CTAGS" rlimit-based-stack-guard

lang_=
ulimit_=
given_=

header()
{
	lang_=$1
	ulimit_=$2
	given_=$3

	if [ -z "$given_" ]; then
		option='no option'
	else
		option="--stack-limit=$given_"
	fi

	printf "\n# ulimit(%s): %s, %s\n" "$lang_" "$ulimit_" "$option"
}

extract()
{
	grep 'ctags\|^qn' \
		| grep -v '\.ctags\|No options will be read from files or environment' \
		| sed -e 's/ctags[^:]*: //' \
		| sed -e 's/(\([0-9]*\))/(LIMIT)/'
}

limit()
{
	(
		ulimit -s ${ulimit_}
		"$@"
	)
}

r=0

header V 512
limit $CTAGS --options=NONE --verbose -o - input.v > /dev/null 2>&1
r=$((r + $?))
limit $CTAGS --options=NONE --verbose -o - input.v 2>&1 \
		| extract

header V 512 131072
limit $CTAGS --options=NONE --verbose --stack-limit=$given_ -o - input.v > /dev/null 2>&1
r=$((r + $?))
limit $CTAGS --options=NONE --verbose --stack-limit=$given_ -o - input.v 2>&1 \
	| extract

header V 64
limit $CTAGS --options=NONE --verbose -o - input.v > /dev/null 2>&1
r=$((r + $?))
limit $CTAGS --options=NONE --verbose -o - input.v 2>&1 \
	| extract

header V 64 32768
limit $CTAGS --options=NONE --verbose --stack-limit=$given_ -o - input.v > /dev/null 2>&1
r=$((r + $?))
limit $CTAGS --options=NONE --verbose --stack-limit=$given_ -o - input.v 2>&1 \
	| extract

header V 64 32769
limit $CTAGS --options=NONE --verbose --stack-limit=$given_ -o - input.v > /dev/null 2>&1
r=$((r + $?))
limit $CTAGS --options=NONE --verbose --stack-limit=$given_ -o - input.v 2>&1 \
	| extract

exit $r
