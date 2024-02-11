# Copyright: 2024 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh
# exit_if_win32 $CTAGS

O="--quiet --options=NONE --extras=+p"

count_ptags=$(${CTAGS} $O -o - input.cpp | grep -F '!_TAG_KIND_DESCRIPTION!C++' | wc -l)
count_list=$(${CTAGS} $O --with-list-header=no --list-kinds-full=C++ | grep '.[ \t]*[^ \t]*yes' | wc -l)

if ! test "$count_ptags" -eq "$count_list"; then
	echo count_ptags: $count_ptags
	echo count_list: $count_list
	${CTAGS} $O -o - input.cpp
	exit 1
fi
exit 0
