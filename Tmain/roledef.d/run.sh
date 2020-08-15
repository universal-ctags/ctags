# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE --langdef=IMAGINARY --kinddef-IMAGINARY=v,variable,variables"

export MSYS2_ARG_CONV_EXCL=--_roledef-IMAGINARY

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
${CTAGS} --_roledef-NOSUCHLANG
${CTAGS} --_roledef-NOSUCHLANG.k=role,roles
${CTAGS} --_roledef-NOSUCHLANG.'{kind}'=role,roles

title '# no kind spec'
${CTAGS} --_roledef-IMAGINARY
${CTAGS} --_roledef-IMAGINARY=
${CTAGS} --_roledef-IMAGINARY=role
${CTAGS} --_roledef-IMAGINARY=role,roles

title '# echo unknown kind letter'
${CTAGS} --_roledef-IMAGINARY.x=           --_force-quit
${CTAGS} --_roledef-IMAGINARY.x=role       --_force-quit
${CTAGS} --_roledef-IMAGINARY.x=role,      --_force-quit
${CTAGS} --_roledef-IMAGINARY.x=role,roles --_force-quit

title '# echo unknown kind name'
${CTAGS} --_roledef-IMAGINARY.'{abc}'=           --_force-quit
${CTAGS} --_roledef-IMAGINARY.'{abc}'=role       --_force-quit
${CTAGS} --_roledef-IMAGINARY.'{abc}'=role,      --_force-quit
${CTAGS} --_roledef-IMAGINARY.'{abc}'=role,roles --_force-quit

title '# wrong char in a kind letter'
${CTAGS} --_roledef-IMAGINARY.'/'=
${CTAGS} --_roledef-IMAGINARY.'%'=
${CTAGS} --_roledef-IMAGINARY.'^'=role
${CTAGS} --_roledef-IMAGINARY.'#'=role,roles
${CTAGS} --_roledef-IMAGINARY.'F'=role,roles
${CTAGS} --_roledef-IMAGINARY.'{'=role,roles
${CTAGS} --_roledef-IMAGINARY.'{v'=role,roles

title '# wrong kind name'
${CTAGS} --_roledef-IMAGINARY.'{file}'=role,roles
${CTAGS} --_roledef-IMAGINARY.'{}'=role,roles
${CTAGS} --_roledef-IMAGINARY.'{#}'=role,roles

title '# empty role name'
${CTAGS} --_roledef-IMAGINARY.v=
${CTAGS} --_roledef-IMAGINARY.v=,
${CTAGS} --_roledef-IMAGINARY.v=,desc
${CTAGS} --_roledef-IMAGINARY.'{variable}'=
${CTAGS} --_roledef-IMAGINARY.'{variable}'=,
${CTAGS} --_roledef-IMAGINARY.'{variable}'=,desc

title '# wrong char in role name'
${CTAGS} --_roledef-IMAGINARY.v=+role+,
${CTAGS} --_roledef-IMAGINARY.'{variable}'=+role+,

title '# empty description'
${CTAGS} --_roledef-IMAGINARY.v=role
${CTAGS} --_roledef-IMAGINARY.v=role,
${CTAGS} --_roledef-IMAGINARY.'{variable}'=role
${CTAGS} --_roledef-IMAGINARY.'{variable}'=role,

title '# role is acceptable but no input file'
${CTAGS} --_roledef-IMAGINARY.v=role,roles
${CTAGS} --_roledef-IMAGINARY.'{variable}'=role,roles

title '# listing with --list-roles'
${CTAGS} --_roledef-IMAGINARY.v=role,roles --list-roles=IMAGINARY
${CTAGS} --_roledef-IMAGINARY.'{variable}'=role,roles \
		 --_roledef-IMAGINARY.v=foos,foods \
		 --list-roles=IMAGINARY

title '# listing with --list-kinds-full'
${CTAGS} --_roledef-IMAGINARY.v=role,roles --list-kinds-full=IMAGINARY
${CTAGS} --_roledef-IMAGINARY.'{variable}'=role,roles \
		 --_roledef-IMAGINARY.v=foos,foods \
		 --list-kinds-full=IMAGINARY

title '# inject a flag separator'
${CTAGS} --_roledef-IMAGINARY.v='role,roles{foo}' --list-roles=IMAGINARY
${CTAGS} --_roledef-IMAGINARY.'{variable}'='role,roles{foo}' --list-roles=IMAGINARY

title '# inject a broken flag separator(1)'
${CTAGS} --_roledef-IMAGINARY.v='role,roles{foo' --list-roles=IMAGINARY
${CTAGS} --_roledef-IMAGINARY.'{variable}'='role,roles{foo' --list-roles=IMAGINARY

title '# inject a broken flag separator(2)'
${CTAGS} --_roledef-IMAGINARY.v='role,roles{' --list-roles=IMAGINARY
${CTAGS} --_roledef-IMAGINARY.'{variable}'='role,roles{' --list-roles=IMAGINARY

title '# use a { in description (1)'
${CTAGS} --_roledef-IMAGINARY.v='role,roles\{' --list-roles=IMAGINARY
${CTAGS} --_roledef-IMAGINARY.'{variable}'='role,roles\{' --list-roles=IMAGINARY

title '# use a { in description (2)'
${CTAGS} --_roledef-IMAGINARY.v='role,roles\{}' --list-roles=IMAGINARY
${CTAGS} --_roledef-IMAGINARY.'{variable}'='role,roles\{}' --list-roles=IMAGINARY

title '# use a \ in description'
${CTAGS} --_roledef-IMAGINARY.v='role,roles\\backslash' --list-roles=IMAGINARY
${CTAGS} --_roledef-IMAGINARY.'{variable}'='role,roles\\backslash' --list-roles=IMAGINARY

title '# description started from {'
${CTAGS} --_roledef-IMAGINARY.v='role,{' --list-roles=IMAGINARY
${CTAGS} --_roledef-IMAGINARY.'{variable}'='role,{' --list-roles=IMAGINARY

title '# description started from \{'
${CTAGS} --_roledef-IMAGINARY.v='role,\{' --list-roles=IMAGINARY
${CTAGS} --_roledef-IMAGINARY.'{variable}'='role,\{' --list-roles=IMAGINARY

title '# too many roles'
opts=
for i in $(seq 0 64); do
	opts="$opts --_roledef-IMAGINARY.v=r$i,desc$i "
done
${CTAGS} $opts

} > /tmp/ctags-tmain-$$.stdout 2>/tmp/ctags-tmain-$$.stderr

sed -e 's/\.exe//g' < /tmp/ctags-tmain-$$.stdout
rm /tmp/ctags-tmain-$$.stdout

sed -e 's/\.exe//g' < /tmp/ctags-tmain-$$.stderr 1>&2
rm /tmp/ctags-tmain-$$.stderr
