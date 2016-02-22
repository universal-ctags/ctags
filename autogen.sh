#!/bin/sh

ctags_files=`make -f makefiles/list-translator-input.mak`
misc/dist-test-cases > makefiles/test-cases.mak && \
    if autoreconf -vfi; then
	if which perl > /dev/null; then
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
