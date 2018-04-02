#!/bin/sh

set -xe

type autoreconf || exit 1
type pkg-config || exit 1

if [ -z "${MAKE}" ]; then
	if type make > /dev/null; then
		MAKE=make
	elif type bmake > /dev/null; then
		MAKE=bmake
	else
		echo "make command is not found" 1>&1
		exit 1
	fi
fi

ctags_files=`${MAKE} -s -f makefiles/list-translator-input.mak`
misc/dist-test-cases > makefiles/test-cases.mak && \
    if autoreconf -vfi; then
	if type perl > /dev/null; then
	    for i in ${ctags_files}; do
		o=${i%.ctags}.c
		echo "optlib2c: translating $i to $o"
		./misc/optlib2c $i > $o
	    done
	else
	    for i in ${ctags_files}; do
		o=${i%.ctags}.c
		echo "use pre-translated file: $o"
	    done
	fi
    fi

exit $?
