# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

#
# This test case doesn't work well on Windows on CI/CD envornments we use.
# We can make a directory having \t in their name.
# cd built-in command with the name works well.
# pwd prints the directory including \t as expected.
# However, the input fields of TAG_PROC_CWD doesn't includ \t
# character; it is replaced with a strange byte sequence "f0 09".
# How to test:
#
#   $ cd ctags
#   $ make
#   $ mkdir "$(printf 'a\tb')"
#   $ cd "$(printf 'a\tb')"
#   $ echo 'int x;' > input.c
#   $ ../ctags --extras=+p -o - input.c | grep _TAG_PROC_CWD
#
# Temporary we disable this test case.
#
exit_if_win32 $CTAGS

O="--quiet --options=NONE "

(
	pid=$$
	dir="$(printf 'ctags-%s\t%s-tmain' $pid $pid)"
	cd /
	if pwd=$(readlink /tmp); then
		cd "${pwd}"
	else
		cd /tmp
	fi
	pwd=$(pwd)
	mkdir "$dir"
	cd "$dir"
	touch input2.c
	ls
	${CTAGS} $O \
			 --extras=+p --pseudo-tags=TAG_PROC_CWD \
			 -o - \
			 input2.c | sed -e "s/$pid//g" | sed -e "s|$pwd|/tmp|" | sed -e "s|[A-Z]:/.*/tmp|/tmp|"
	rm input2.c
	cd ..
	rmdir "$dir"
)
