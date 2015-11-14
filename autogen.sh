#!/bin/sh
misc/dist-test-cases > makefiles/test-cases.mak && \
autoreconf -vfi && {
    for i in `make -f makefiles/list-translator-input.mak`; do
	o=${i%.ctags}.c
	echo "optlib2c: translating $i to $o"
	./misc/optlib2c $i > $o
    done
}

exit $?
