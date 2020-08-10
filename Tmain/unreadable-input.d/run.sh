#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

if [ $(id -u) = 0 ] && ! sudo -u '#1' $CTAGS --version > /dev/null; then
	skip "sudo needed in this test case doesn't work expectedly on this platform (execution)"
fi

echo "int v0;" > input-0.c
chmod a-r input-0.c

if [ $(id -u) = 0 ] && sudo -u '#1' cat input-0.c > /dev/null 2>&1; then
	rm input-0.c
	skip "sudo needed in this test case doesn't work expectedly on this platform (file reading)"
elif [ $(id -u) != 0 ] && cat input-0.c > /dev/null 2>&1; then
	rm input-0.c
	skip "chmod a-r doesn't work expectedly on this platform"
fi


{
	echo input-0.c
	echo input-1.c
} | {
	if [ $(id -u) = 0 ]; then
		# The root can read a file even we did "chmod a-r".
		sudo -u '#1' $CTAGS --quiet --options=NONE -L - -o -
	else
		$CTAGS --quiet --options=NONE -L - -o -
	fi
}
rm input-0.c
