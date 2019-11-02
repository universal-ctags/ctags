# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"
BUILDDIR=$2

$CTAGS --options=X.ctags --list-kinds=X

echo "# no kind is specified" 1>&2
$CTAGS --options=Ynokind.ctags -o $BUILDDIR/tags input.yyy

echo "# no kind (empty,) is specified" 1>&2
$CTAGS --options=Yempty1.ctags -o $BUILDDIR/tags input.yyy

echo "# no kind (empty,,) is specified" 1>&2
$CTAGS --options=Yempty2.ctags -o $BUILDDIR/tags input.yyy

echo "# (r/) kind is specified inline" 1>&2
$CTAGS --options=Y-r.ctags -o $BUILDDIR/tags input.yyy

echo "# (/regex) kind is specified inline" 1>&2
$CTAGS --options=Y-regex.ctags -o $BUILDDIR/tags input.yyy

echo "# (r/regex) kind is specified inline" 1>&2
$CTAGS --options=Y-r-regex.ctags -o $BUILDDIR/tags input.yyy

echo "# (r/regex) kind defined with --kinddef-<LANG> option" 1>&2
$CTAGS --options=Y-r-regex-with-kinddef.ctags -o $BUILDDIR/tags input.yyy
