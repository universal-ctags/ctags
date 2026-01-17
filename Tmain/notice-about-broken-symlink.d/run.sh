# Copyright: 2025 Masatake YAMATO
# License: GPL-2

. ../utils.sh

CTAGS=$1
SF=broken-symlink.c

# See https://www.msys2.org/docs/symlinks/
skip_if_running_on_msys

cleanup()
{
	rm -f "$SF"
}

trap cleanup EXIT

cleanup
if ! ln -s no-such-file.c "$SF"; then
	skip "failed to create a symbolic link"
fi

echo '# broken-symlink.c + no option' 1>&2
$CTAGS --options=NONE -o - broken-symlink.c

echo '# broken-symlink.c + --quiet option' 1>&2
$CTAGS --quiet --options=NONE -o - broken-symlink.c

echo '# no-such-file.c + no option' 1>&2
$CTAGS --options=NONE -o - no-such-file.c

echo '# no-such-file.c + --quiet option' 1>&2
$CTAGS --quiet --options=NONE -o - no-such-file.c
