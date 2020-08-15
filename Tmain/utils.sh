__SKIP__=77

skip()
{
	echo "$@"
	exit ${__SKIP__}
}

remove_commit_id()
{
    # Remove a commit id embedded in tags file
    sed -i -e '/!_TAG_PROGRAM_VERSION.*/s#/[^/]*/#//#' $1
}

filesize()
{
    wc -c < "$1"
}

is_feature_available()
{
    local ctags=$1
	local tmp=$2
	local o="--quiet --options=NONE"
	local neg
	local feat

	if [ "${tmp}" = '!' ]; then
		neg=1
		feat=$3
	else
		feat=$2
	fi

	if [ "${neg}" = 1 ]; then
		if ${ctags} $o --with-list-header=no --list-features | grep -q "$feat"; then
			skip "feature \"$feat\" is available in $ctags"
		fi
	else
		if ! ${ctags} $o --with-list-header=no --list-features | grep -q "$feat"; then
			skip "feature \"$feat\" is not available in $ctags"
		fi
	fi
}

skip_if_user_has_dot_ctags_d()
{
	if [ -d ~/.ctags.d ]; then
		skip "this test case doesn't work well if you have ~/.ctags.d"
	fi
}

exit_if_win32()
{
	is_feature_available $1 '!' win32
}

exit_unless_win32()
{
	is_feature_available $1 win32
}

exit_if_no_case_insensitive_filenames()
{
	is_feature_available $1 case-insensitive-filenames
}

run_with_format()
{
    echo '#' $*
    local format=$1
    shift
    ${CTAGS} --quiet --options=NONE --output-format=$format "$@" -o - input.*
}

exit_status_for_input_c()
{
	local ctags=$1
	shift

	local remove_file=$1
	shift

	printf "%s => " "$(echo "$*" | sed -e 's#[^ ][^ ]*/\([^ ]*\)#\1#g')"
	${ctags} --quiet --options=NONE "$@" input.c > /dev/null
	local result_local=$?

	if [ "$remove_file" != "none" ]; then
		rm -f "$remove_file"
	fi

	if [ "$result_local" = 0 ]; then
		echo "ok"
	else
		echo "failed"
	fi
}

get_column_index()
{
	local index=0
	local ctags=$1
	local option=$2
	local column=$3

	for x in $($ctags  --quiet --options=NONE --with-list-header "$option" | sed -ne 's/^#\(.*\)$/\1/p'); do
		if [ "$x" = "$column" ]; then
			echo $index
			return 0
		fi
		index=$(expr $index + 1)
	done

	echo -1
	return 1
}

filter_by_column_index()
{
	local index=$1

	awk '{print $'$(expr $index + 1)'}'
}

echo2()
{
	# use a external echo command here.
	# built-in echo suppresses \1.
	/bin/echo "$@"
	/bin/echo "$@" 1>&2
}

direq ()
{
    [ "$(cd ${1} && pwd)" = "$(cd ${2} && pwd)" ]
    return $?
}
