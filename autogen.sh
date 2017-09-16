#!/bin/sh

set -xe

type autoreconf || exit 1
type pkg-config || exit 1

gen_bsdmakefile() {
	set +xe
	echo -e '.DONE:\n\t@echo "Please use GNU Make (gmake) to build this project"\n.DEFAULT:\n\t@echo "Please use GNU Make (gmake) to build this project"' > BSDmakefile
	set -xe
}

is_bsd() {
	UNAME=`uname`
	if echo $UNAME | grep -i bsd >/dev/null; then
		#return true
		return 0
	else
		#return false
		return 1
	fi
}

if is_bsd; then
	MAKE="gmake"
	gen_bsdmakefile
else
	MAKE="make"
fi

ctags_files=`${MAKE} -f makefiles/list-translator-input.mak --no-print-directory`
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
