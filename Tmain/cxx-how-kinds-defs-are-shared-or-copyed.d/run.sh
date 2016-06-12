# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

printf "[C] enabling: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C=+f --list-kinds=C | grep ^f

printf "[C] disbaling: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C=-f --list-kinds=C | grep ^f

printf "[C++] enabling: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C=+f --list-kinds=C++ | grep ^f

printf "[C++] disbaling: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C=-f --list-kinds=C++ | grep ^f
