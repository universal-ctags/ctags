# Copyright: 2025 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1

echo '# broken-symlink.c + no option' 1>&2
$CTAGS --options=NONE -o - broken-symlink.c

echo '# broken-symlink.c + --quiet option' 1>&2
$CTAGS --quiet --options=NONE -o - broken-symlink.c

echo '# no-such-file.c + no option' 1>&2
$CTAGS --options=NONE -o - no-such-file.c

echo '# no-such-file.c + --quiet option' 1>&2
$CTAGS --quiet --options=NONE -o - no-such-file.c
