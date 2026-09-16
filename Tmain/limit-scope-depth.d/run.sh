# Copyright: 2026 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --sort=no -o - input.js
