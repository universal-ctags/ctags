#!/bin/sh

set -xe

type autoreconf > /dev/null 2>&1 || {
	echo "No autotools (autoconf and automake) found" 1>&2
	exit 1
}
type pkg-config > /dev/null 2>&1 || {
	echo "No pkg-config found" 1>&2
	exit 1
}

if [ -z "${MAKE}" ]; then
	if type make > /dev/null; then
		MAKE=make
	elif type bmake > /dev/null; then
		MAKE=bmake
	else
		echo "make command is not found" 1>&2
		exit 1
	fi
fi

ctags_files=`${MAKE} -s -f makefiles/list-optlib2c-input.mak`
txt_files=`${MAKE} -s -f makefiles/list-txt2cstr-input.mak`
if autoreconf -vfi; then
	if type perl > /dev/null; then
		for i in ${ctags_files}; do
			o=${i%.ctags}.c
			echo "optlib2c: translating $i to $o"
			if ! ./misc/optlib2c $i > $o; then
				echo "failed in running optlib2c" 1>&2
				exit 1
			fi
		done
		for i in ${txt_files}; do
			o=${i%.ps}.c
			echo "txt2cstr: converting $i to $o"
			if ! ./misc/txt2cstr $i > $o; then
				echo "failed in running txt2cstr" 1>&2
				exit 1
			fi
		done

	else
		for i in ${ctags_files}; do
			o=${i%.ctags}.c
			echo "use pre-translated file: $o"
		done
		for i in ${txt_files}; do
			o=${i%.ps}.c
			echo "use pre-converted file: $o"
		done
	fi
else
	echo "failed in running autoreconf" 1>&2
	exit 1
fi

exit $?
