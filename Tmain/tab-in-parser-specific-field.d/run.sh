# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

run()
{
	echo '#'
	echo '#' with $1
	echo '#'

	echo "# Universal-ctags output format"
	$CTAGS --options=NONE --options=./foo.ctags "$1" --output-format=u-ctags  -o - input.foo

	echo "# Exuberant-ctags output format"
	echo "# 'a' has a tab char in fprop fields. So ctags drops it silent."
	$CTAGS --options=NONE --options=./foo.ctags "$1" --output-format=e-ctags  -o - input.foo
}

run "--sort=yes"
run "--sort=no"
