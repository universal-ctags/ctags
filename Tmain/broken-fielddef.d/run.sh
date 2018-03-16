# Copyright: 2017 Masatake YAMATO
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
${CTAGS} --_fielddef-NOSUCHLANG
${CTAGS} --_fielddef-NOSUCHLANG=field,desc

title '# no option value'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=

title '# wrong char in a field name'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=:
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=:abc
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=:abc,
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=:abc,description

title '# empty field name'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=,
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=,abc
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=,abc,
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=,abc,description

title '# empty description'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=abc
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=abc,

title '# no input file'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=abc,desc

title '# inject a flag separator'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc{foo}' --list-fields=IMAGINARY

title '# inject a broken flag separator(1)'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc{foo' --list-fields=IMAGINARY

title '# inject a broken flag separator(2)'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc{' --list-fields=IMAGINARY

title '# use a { in description (1)'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc\{' --list-fields=IMAGINARY

title '# use a { in description (2)'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc\{}' --list-fields=IMAGINARY

title '# use a \ in description'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc\\backslash' --list-fields=IMAGINARY

title '# description started from {'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,{' --list-fields=IMAGINARY

title '# description started from \{'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,\{' --list-fields=IMAGINARY
}  > /tmp/ctags-tmain-$$.stdout 2>/tmp/ctags-tmain-$$.stderr

sed -e 's/\.exe//g' < /tmp/ctags-tmain-$$.stdout
rm /tmp/ctags-tmain-$$.stdout

sed -e 's/\.exe//g' < /tmp/ctags-tmain-$$.stderr 1>&2
rm /tmp/ctags-tmain-$$.stderr
