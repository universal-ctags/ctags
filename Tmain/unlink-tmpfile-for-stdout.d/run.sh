# Copyright: 2026 Keegan Smith
# License: GPL-2

CTAGS=$1
BUILDDIR=$2

. ../utils.sh

# Windows doesn't allow removing a file that is still open. There,
# ctags keeps the temporary file until it closes the file.
exit_if_win32 "$CTAGS"

D=$BUILDDIR/unlink-tmpfile-for-stdout.tmp
rm -rf "$D"
mkdir -p "$D/tmp"

# With "-o -", ctags writes tags to a temporary file in $TMPDIR and
# copies them to stdout at the end. The temporary file should be
# removed right after creating it so that a killed ctags process
# doesn't leave it behind.
#
# ctags opens the tag file before reading the file list given with
# "-L -", and reports "Reading list file" with -V after that. Wait for
# the message, then look into $TMPDIR while ctags is still running.
{
	i=0
	while ! grep -q 'Reading list file' "$D/stderr" 2>/dev/null; do
		i=$((i + 1))
		if [ $i -gt 30 ]; then
			break
		fi
		sleep 1
	done
	ls "$D/tmp" > "$D/ls"
	echo input.c
} | TMPDIR="$D/tmp" ${CTAGS} --options=NONE -V -o - --extras=-p -L - 2> "$D/stderr"

echo '# files in TMPDIR while ctags is running'
cat "$D/ls"
echo '# files in TMPDIR after ctags exits'
ls "$D/tmp"

rm -rf "$D"
