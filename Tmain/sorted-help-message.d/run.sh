# Copyright: 2015 Masatake YAMATO
# License: GPL-2

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

extract_short_options()
{
    sed -n '/Usage:/,$p'  | \
    sed -n 's/\(^  -[[:alnum:]]\).*/\1/p'
}

extract_long_options()
{
    sed -n '/Usage:/,$p'  | \
    sed -n 's/\(^  --[:alnum:][<>[:alnum:]_-]*\).*/\1/p'
}

extract_debug_options()
{
    sed -n '/Usage:/,$p'  | \
    sed -n 's/\(^  --_[<>[:alnum:]_-]*\).*/\1/p'
}



diff \
    <(print_help | extract_short_options) \
    <(print_help | extract_short_options | opt_sort) &&

diff \
    <(print_help | extract_long_options) \
    <(print_help | extract_long_options | opt_sort) &&

diff \
    <(print_help | extract_debug_options) \
    <(print_help | extract_debug_options | opt_sort) &&

exit $?
