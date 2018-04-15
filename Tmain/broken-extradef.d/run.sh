# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

title()
{
	echo
	echo "$@"

	{
		echo
		echo "$@"
	} 1>&2
}

{
title '# echo unknown lang'
${CTAGS} --_extradef-NOSUCHLANG
${CTAGS} --_extradef-NOSUCHLANG=extra,desc

title '# no option value'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=

title '# wrong char in a field name'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=:
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=:abc
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=:abc,
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=:abc,description

title '# empty extra name'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=,
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=,abc
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=,abc,
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=,abc,description

title '# empty description'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=abc
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=abc,

title '# no input file'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY=abc,desc

title '# inject a flag separator'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY='extra,desc{foo}' --list-extras=IMAGINARY

title '# inject a broken flag separator(1)'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY='extra,desc{foo' --list-extras=IMAGINARY

title '# inject a broken flag separator(2)'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY='extra,desc{' --list-extras=IMAGINARY

title '# use a { in description (1)'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY='extra,desc\{' --list-extras=IMAGINARY

title '# use a { in description (2)'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY='extra,desc\{}' --list-extras=IMAGINARY

title '# use a \ in description'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY='extra,desc\\backslash' --list-extras=IMAGINARY

title '# description started from {'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY='extra,{' --list-extras=IMAGINARY

title '# description started from \{'
${CTAGS} --langdef=IMAGINARY --_extradef-IMAGINARY='extra,\{' --list-extras=IMAGINARY

} > /tmp/ctags-tmain-$$.stdout 2>/tmp/ctags-tmain-$$.stderr

sed -e 's/\.exe//g' < /tmp/ctags-tmain-$$.stdout
rm /tmp/ctags-tmain-$$.stdout

sed -e 's/\.exe//g' < /tmp/ctags-tmain-$$.stderr 1>&2
rm /tmp/ctags-tmain-$$.stderr
