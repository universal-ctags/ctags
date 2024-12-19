# Copyright: 2024 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

# The order of stdout and stderr lines is not stable.
exit_if_win32 "$CTAGS"

# is_feature_available $CTAGS json

O="--options=NONE --language-force=CTagsSelfTest"

for fmt in xref; do
	echo "# no extra ($fmt)"
	${CTAGS} $O -o - --output-format="$fmt" input.cst 2>&1

	echo "# drop '0' extra ($fmt)"
	${CTAGS} $O -o - --output-format="$fmt" --extras=-z input.cst 2>&1

	echo "# drop '{nulltag}' extra ($fmt)"
	${CTAGS} $O -o - --output-format="$fmt" --extras=-'{nulltag}' input.cst 2>&1

	echo '# with --extras=+0 ($fmt)'
	${CTAGS} $O -o - --output-format="$fmt" --extras=+z input.cst 2>&1

	echo "# with --extras=+{nulltag}' ($fmt)"
	${CTAGS} $O -o - --output-format="$fmt" --extras=+'{nulltag}' input.cst 2>&1
done | sed -e 's/\.exe//'
