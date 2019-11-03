# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

. ../utils.sh

if ! sort --help | grep --quiet GNU; then
    skip "GNU sort is needed to run this test case"
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
    sed -n 's/\(^  --[[:alnum:]][<>[:alnum:]_-]*\).*/\1/p'
}

extract_debug_options()
{
    sed -n '/Usage:/,$p'  | \
    sed -n 's/\(^  --_[<>[:alnum:]_-]*\).*/\1/p'
}


gdiff()
{
    print_help | extract_$1_options > $BUILDDIR/$1.tmp
    print_help | extract_$1_options | opt_sort > $BUILDDIR/sorted-$1.tmp

    diff -ruN $BUILDDIR/$1.tmp $BUILDDIR/sorted-$1.tmp
    r=$?
    rm $BUILDDIR/$1.tmp $BUILDDIR/sorted-$1.tmp

    return $r
}

gdiff short && gdiff long  && gdiff debug
exit $?
