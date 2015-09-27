#!/bin/sh -e
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
