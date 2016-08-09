# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

printf "[C] enabling in C: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C=+f --list-kinds=C | grep ^f

printf "[C] disabling in C: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C=-f --list-kinds=C | grep ^f

printf "[C++] enabling in C: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C=+f --list-kinds=C++ | grep ^f

printf "[C++] disabling in C: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C=-f --list-kinds=C++ | grep ^f

printf "[C] enabling in C++: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C++=+f --list-kinds=C | grep ^f

printf "[C] disabling in C++: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C++=-f --list-kinds=C | grep ^f

printf "[C++] enabling in C++: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C++=+f --list-kinds=C++ | grep ^f

printf "[C++] disabling in C++: "
${CTAGS} --quiet --options=NONE \
	 --kinds-C++=-f --list-kinds=C++ | grep ^f
