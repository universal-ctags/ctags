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
    local feat=$2

    if ! ${ctags} --list-features | grep -q "$feat"; then
		skip "feature \"$feat\" is not available in $ctags"
    fi
}

exit_if_no_coproc()
{
    is_feature_available $1 coproc
}

run_with_format()
{
    echo '#' $*
    local format=$1
    shift
    ${CTAGS} --quiet --options=NONE --output-format=$format "$@" -o - input.*
}
