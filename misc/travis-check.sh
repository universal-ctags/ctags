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

# Our sandbox doesn't work with gcov.
# Following cases target ctags without gcov.
SANDBOX_CASES=sandbox,sandbox-crash,sandbox-default-req,sandbox-unknown-submode


if [ "$TARGET" = "Unix" ]; then
    ./autogen.sh
    CONFIGURE_CMDLINE="../configure --enable-debugging --enable-iconv "

    BUILDDIR0="$TRAVIS_OS_NAME"-"$CC"
    if [ "$TRAVIS_OS_NAME" = "linux" ] && [ "$CC" = "gcc" ]; then
		if ! git diff --exit-code optlib; then
			echo "Files under optlib are not up to date."
			echo "If you change optlib/foo.ctags, don't forget to add optlib/foo.c to your commit."
			exit 1
		fi

        BUILDDIR=${BUILDDIR0}-gcov
        mkdir -p "${BUILDDIR}"
        (
            cd "${BUILDDIR}"
            ${CONFIGURE_CMDLINE} --enable-coverage-gcov
            make -j2
            echo 'List features'
            ./ctags --list-features
            echo 'Run "make check" with gcov'
            make check roundtrip TRAVIS=1
			make dist
			tar zxvf universal-ctags*tar.gz
			(
				cd universal-ctags*[0-9]
				BUILDDIR=${BUILDDIR0}
				mkdir -p "${BUILDDIR}"
				(
					cd "${BUILDDIR}"
					${CONFIGURE_CMDLINE}
					make -j2
					echo 'Run "make tmain (sandbox only)" without gcov'
					make tmain TRAVIS=1 UNITS=${SANDBOX_CASES}

					make clean
				)
			)
        )

    else
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
