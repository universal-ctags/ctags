# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1

echo '# Z only'
$CTAGS --quiet --options=NONE --fields=Z -o - input.c 2>&1 \
	| sed -e 's/\.exe//g' \
	| sort

# The last sort is needed because the order of lines from stdout and
# that from stderr is not stable.
