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
${CTAGS} --_paramdef-NOSUCHLANG
${CTAGS} --_paramdef-NOSUCHLANG=param,desc

title '# no option value'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=

title '# wrong char in a field name'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=:
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=:abc
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=:abc,
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=:abc,description

title '# empty parameter name'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=,
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=,abc
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=,abc,
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=,abc,description

title '# empty description'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=abc
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=abc,

title '# no input file'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY=abc,desc

title '# inject a flag separator'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY='param,desc{foo}' --list-params=IMAGINARY

title '# inject a broken flag separator(1)'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY='param,desc{foo' --list-params=IMAGINARY

title '# inject a broken flag separator(2)'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY='param,desc{' --list-params=IMAGINARY

title '# use a { in description (1)'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY='param,desc\{' --list-params=IMAGINARY

title '# use a { in description (2)'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY='param,desc\{}' --list-params=IMAGINARY

title '# use a \ in description'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY='param,desc\\backslash' --list-params=IMAGINARY

title '# description started from {'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY='param,{' --list-params=IMAGINARY

title '# description started from \{'
${CTAGS} --langdef=IMAGINARY --_paramdef-IMAGINARY='param,\{' --list-params=IMAGINARY

} > /tmp/ctags-tmain-$$.stdout 2>/tmp/ctags-tmain-$$.stderr

sed -e 's/\.exe//g' < /tmp/ctags-tmain-$$.stdout
rm /tmp/ctags-tmain-$$.stdout

sed -e 's/\.exe//g' < /tmp/ctags-tmain-$$.stderr 1>&2
rm /tmp/ctags-tmain-$$.stderr
