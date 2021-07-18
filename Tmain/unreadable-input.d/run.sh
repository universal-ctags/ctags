#!/bin/sh

# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1
INPUT0=/tmp/u-ctags/input-0.c

. ../utils.sh

if ! type pidof > /dev/null 2>&1; then
	# pidof is needed to find auditd.
	skip "pidof command is not available"
fi

if ! pidof auditd > /dev/null 2>&1; then
	# sudo expects auditd is running.
	skip "auditd is not running"
fi

if [ $(id -u) = 0 ] && ! sudo -u '#1' $CTAGS --version > /dev/null; then
	skip "sudo needed in this test case doesn't work expectedly on this platform (execution)"
fi

mkdir -p ${INPUT0%/*}
echo "int v0;" > ${INPUT0}
chmod a-r ${INPUT0}

if [ $(id -u) = 0 ] && sudo -u '#1' cat ${INPUT0} > /dev/null 2>&1; then
	rm ${INPUT0}
	skip "sudo needed in this test case doesn't work expectedly on this platform (file reading)"
elif [ $(id -u) != 0 ] && cat ${INPUT0} > /dev/null 2>&1; then
	rm ${INPUT0}
	skip "chmod a-r doesn't work expectedly on this platform"
fi


{
	echo ${INPUT0}
	echo input-1.c
} | {
	if [ $(id -u) = 0 ]; then
		# The root can read a file even we did "chmod a-r".
		sudo -u '#1' $CTAGS --quiet --options=NONE -L - -o -
	else
		$CTAGS --quiet --options=NONE -L - -o -
	fi
}
rm ${INPUT0}
