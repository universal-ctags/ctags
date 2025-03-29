# Copyright: 2025 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

$CTAGS --options=./broken-pattern.ctags -o - input.py > /dev/null
