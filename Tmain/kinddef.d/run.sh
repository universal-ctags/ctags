# Copyright: 2016 Masatake YAMATO
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
title '# NOSUCHLANG'
${CTAGS} --kinddef-NOSUCHLANG=l,name,description --list-kinds-full=NOSUCHLANG

CTAGS="${CTAGS} --langdef=MYTEST"

title '# define "a"'
${CTAGS} --kinddef-MYTEST=a,aa,aaa --list-kinds-full=MYTEST

title '# no description 1'
${CTAGS} --kinddef-MYTEST=a,aa, --list-kinds-full=MYTEST

title '# no description 2'
${CTAGS} --kinddef-MYTEST=a,aa --list-kinds-full=MYTEST

title '# no name 1'
${CTAGS} --kinddef-MYTEST=a, --list-kinds-full=MYTEST

title '# no name 2'
${CTAGS} --kinddef-MYTEST=a --list-kinds-full=MYTEST

title '# an empty name 1'
${CTAGS} --kinddef-MYTEST=a,,x --list-kinds-full=MYTEST

title '# an empty name 2'
${CTAGS} --kinddef-MYTEST=a,, --list-kinds-full=MYTEST

title '# wrong letter in name 1'
${CTAGS} --kinddef-MYTEST="a,a	x,d" --list-kinds-full=MYTEST

title '# wrong letter in name 2'
${CTAGS} --kinddef-MYTEST="a,a	x," --list-kinds-full=MYTEST

title '# wrong letter in name 3'
${CTAGS} --kinddef-MYTEST="a,a	x" --list-kinds-full=MYTEST

title '# no letter 1'
${CTAGS} --kinddef-MYTEST=,n,d --list-kinds-full=MYTEST

title '# no letter 2'
${CTAGS} --kinddef-MYTEST=,n, --list-kinds-full=MYTEST

title '# no letter 3'
${CTAGS} --kinddef-MYTEST=,n --list-kinds-full=MYTEST

title '# no letter 4'
${CTAGS} --kinddef-MYTEST=, --list-kinds-full=MYTEST

title '# no letter 5'
${CTAGS} --kinddef-MYTEST= --list-kinds-full=MYTEST

title '# wrong letter'
${CTAGS} --kinddef-MYTEST=^ --list-kinds-full=MYTEST

title '# wrong letter (using number)'
${CTAGS} --kinddef-MYTEST=7 --list-kinds-full=MYTEST

title '# reusing the letter for file kind'
${CTAGS} --kinddef-MYTEST=F --list-kinds-full=MYTEST

title '# reusing the name for file kind'
${CTAGS} --kinddef-MYTEST=x,file,desc --list-kinds-full=MYTEST

title '# inject a flag separator'
${CTAGS} --kinddef-MYTEST='x,kind,desc{foo}' --list-kinds-full=MYTEST

title '# inject a broken flag separator(1)'
${CTAGS} --kinddef-MYTEST='x,kind,desc{foo' --list-kinds-full=MYTEST

title '# inject a broken flag separator(2)'
${CTAGS} --kinddef-MYTEST='x,kind,desc{' --list-kinds-full=MYTEST

title '# use a { in description (1)'
${CTAGS} --kinddef-MYTEST='x,kind,desc\{' --list-kinds-full=MYTEST

title '# use a { in description (2)'
${CTAGS} --kinddef-MYTEST='x,kind,desc\{}' --list-kinds-full=MYTEST

title '# use a number char as the initial letter'
${CTAGS} --kinddef-MYTEST='x,0kind,desc' --list-kinds-full=MYTEST

title '# use a number char within the body'
${CTAGS} --kinddef-MYTEST='x,k0ind,desc' --list-kinds-full=MYTEST

# title '# use a { and \t in description'
# ${CTAGS} --kinddef-MYTEST='x,kind,desc\{}\t' --list-kinds-full=MYTEST

title '# use a \ in description'
${CTAGS} --kinddef-MYTEST='x,kind,desc\\backslash' --list-kinds-full=MYTEST

title '# description started from {'
${CTAGS} --kinddef-MYTEST='x,kind,{' --list-kinds-full=MYTEST

title '# description started from \{'
${CTAGS} --kinddef-MYTEST='x,kind,\{' --list-kinds-full=MYTEST

title '# _refonly flag'
${CTAGS} --kinddef-MYTEST='x,kind,desc' --list-kinds-full=MYTEST
${CTAGS} --kinddef-MYTEST='x,kind,desc' --_roledef-MYTEST.x=role,roleDesc --list-kinds-full=MYTEST
${CTAGS} --kinddef-MYTEST='x,kind,desc{_refonly}' --_roledef-MYTEST.'{kind}'=role,roleDesc --list-kinds-full=MYTEST

} > /tmp/ctags-tmain-$$.stdout 2>/tmp/ctags-tmain-$$.stderr

sed -e 's/\.exe//g' < /tmp/ctags-tmain-$$.stdout
rm /tmp/ctags-tmain-$$.stdout

sed -e 's/\.exe//g' < /tmp/ctags-tmain-$$.stderr 1>&2
rm /tmp/ctags-tmain-$$.stderr
