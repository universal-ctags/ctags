#!/bin/sh

# Copyright: 2022 Masatake YAMATO
# License: GPL-2

CTAGS=$1
READTAGS=$3
INPUT=$(printf "a\tb\nc")

. ../utils.sh

exit_if_win32

if ! [ -x "${READTAGS}" ]; then
	skip "no readtags"
fi

mkdir -p "${INPUT}"
echo "int x;" > "${INPUT}/f.c"

O="--quiet --options=NONE -o - -R --fields=+z"
echo "# ctags:"
${CTAGS} $O "${INPUT}"

echo "# ctags | readtags:"
${CTAGS} $O "${INPUT}" | ${READTAGS} -eE -t - -l

rm -f "${INPUT}/f.c"
rmdir "${INPUT}"
