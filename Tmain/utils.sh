__SKIP__=77

remove_commit_id()
{
    # Remove a commit id embedded in tags file
    sed -i -e '/!_TAG_PROGRAM_VERSION.*/s#/[^/]*/#//#' $1
}

is_feature_available()
{
    local ctags=$1
    local feat=$2

    if ! ${ctags} --list-features | grep -q "$feat"; then
	echo "feature \"$feat\" is not available in $ctags"
	exit ${__SKIP__}
    fi
}

exit_if_no_coproc()
{
    is_feature_available $1 coproc
}
