# Copyright: 2025 Masatake YAMATO
# License: GPL-2

CTAGS=$1

. ../utils.sh

echo "# < macros.d/macros.vm"
${CTAGS} --quiet --options=NONE --print-language macros.d/macros.vim
${CTAGS} --quiet --options=NONE -o - macros.d/macros.vim

echo "# cd macros.d; < macros.vim"
(
	cd macros.d;
	${CTAGS} --quiet --options=NONE --print-language macros.vim;
	${CTAGS} --quiet --options=NONE -o - macros.vim
)
