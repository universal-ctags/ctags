CTAGS=$1

if ! [ "$(basename $SHELL)" = "bash" ]; then
    echo "bash is needed to run this test case"
    exit 77
fi

if ! sort --help | grep --quiet GNU; then
    echo "GNU sort is needed to run this test case"
    exit 77
fi

print_help()
{
    ${CTAGS} --quiet --options=NONE --help
}

opt_sort()
{
    sort --ignore-case --stable
}

extrac_short_options()
{
    sed -n '/Usage:/,$p'  | \
    sed -n 's/\(^  -[[:alnum:]]\).*/\1/p'
}

extrac_long_options()
{
    sed -n '/Usage:/,$p'  | \
    sed -n 's/\(^  --[[:alnum:]_-<>]\).*/\1/p'
}

extrac_debug_options()
{
    sed -n '/Usage:/,$p'  | \
    sed -n 's/\(^  --_[[:alnum:]_-<>]\).*/\1/p'
}



diff \
    <(print_help | extrac_short_options) \
    <(print_help | extrac_short_options | opt_sort) &&

diff \
    <(print_help | extrac_long_options) \
    <(print_help | extrac_long_options | opt_sort) &&

diff \
    <(print_help | extrac_debug_options) \
    <(print_help | extrac_debug_options | opt_sort) &&

exit $?
