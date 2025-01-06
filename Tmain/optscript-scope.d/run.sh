# Copyright: 2024 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

${CTAGS} --quiet --options=NONE \
		 --sort=no --fields=+n \
		 --options=./unknown.ctags \
		 --language-force=UnknownX \
		 -o - input.unknown
