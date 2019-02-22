# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

echo "# Universal-ctags output format"
$CTAGS --options=NONE --options=./foo.ctags --output-format=u-ctags  -o - input.foo

echo "# Exuberant-ctags output format"
echo "# fun1 has a tab char in fprop fields. So ctags drops it silent."
$CTAGS --options=NONE --options=./foo.ctags --output-format=e-ctags  -o - input.foo
