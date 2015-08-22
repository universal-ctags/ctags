#!/bin/sh -e

autoreconf -f -i -v
./configure --enable-iconv

if [ "$TARGET" = "Unix" ]; then
    if [ "$TRAVIS_OS_NAME" = "linux" ] && [ "$CC" = "gcc" ]; then
        make -j2 COVERAGE=1
    else
        make -j2
    fi
    make -j2 check TRAVIS=1
elif [ "$TARGET" = "Mingw32" ]; then
    make -j2 CC=i686-w64-mingw32-gcc -f mk_mingw.mak
    # Don't run test units in Mingw32 target
else
    echo 'Invalid $TARGET value' 1>&2
    exit 1
fi
