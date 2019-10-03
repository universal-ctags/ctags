# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

$CTAGS --options=X.ctags --list-kinds=X

echo "# no kind is specified" 1>&2
$CTAGS --options=Ynokind.ctags input.yyy

echo "# no kind (empty,) is specified" 1>&2
$CTAGS --options=Yempty1.ctags input.yyy

echo "# no kind (empty,,) is specified" 1>&2
$CTAGS --options=Yempty2.ctags input.yyy

echo "# (r/) kind is specified inline" 1>&2
$CTAGS --options=Y-r.ctags input.yyy

echo "# (/regex) kind is specified inline" 1>&2
$CTAGS --options=Y-regex.ctags input.yyy

echo "# (r/regex) kind is specified inline" 1>&2
$CTAGS --options=Y-r-regex.ctags input.yyy

echo "# (r/regex) kind defined with --kinddef-<LANG> option" 1>&2
$CTAGS --options=Y-r-regex-with-kinddef.ctags input.yyy
