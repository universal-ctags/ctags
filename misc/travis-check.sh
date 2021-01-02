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

make --version

if [ "$TARGET" = "Unix" ]; then
    ./autogen.sh
    if [ "$TRAVIS_OS_NAME" = "linux" ] && [ "$CC" = "gcc" ]; then
		if ! git diff --exit-code optlib; then
			echo "Files under optlib are not up to date."
			echo "If you change optlib/foo.ctags, don't forget to add optlib/foo.c to your commit."
			exit 1
		fi
		make -B -C man QUICK=1 update-docs
		if ! git diff --exit-code docs/man; then
			echo "Files under docs/man/ are not up to date."
			echo "Please execute 'make -C man QUICK=1 update-docs' and commit them."
			exit 1
		fi
    fi

elif [ "$TARGET" = "Mingw32" ]; then
    # Don't run test units in Mingw32 target.
    make -j2 CC=i686-w64-mingw32-gcc WINDRES=i686-w64-mingw32-windres CC_FOR_PACKCC=gcc -f mk_mingw.mak

else
    echo "Invalid TARGET value: $TARGET" 1>&2
    exit 1
fi
