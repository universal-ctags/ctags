# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS="$1"

msgstr ()
{
	if [ $1 = '+' ]; then
		echo "enabled"
	else
		echo "disabled"
	fi
}

run_ctags ()
{
	printf '# []: %s, {}: %s\n' $(msgstr $1) $(msgstr $2)
	${CTAGS} --quiet --options=NONE --options=mtextra.ctags \
			 --fields=+'{extras}' \
			 --extras-MTExtra=${1}'{acceptSquareBracket}' \
			 --extras-MTExtra=${2}'{acceptCurlyBracket}' \
			 -o - \
			 input.mtextra
}

run_ctags + +
run_ctags - -
run_ctags + -
run_ctags - +
