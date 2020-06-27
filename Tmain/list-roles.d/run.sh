# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

title()
{
    echo
    echo '#'
    for x in "$@"; do
        echo '#' $x
    done
    echo '#'
}

ignore_xml()
{
    grep -v 'Glade\|Ant\|Maven2\|XSLT'
}

ignore_yaml()
{
    grep -v 'Yaml'
}

# When introducing newly rewritten parser, we would like to provide
# the both new parser and old parser for debugging and providing
# migration period to users. In such case the prefix "Old" will be
# used to the name of old parser. The old parser should be ignored
# in this test case.
ignore_old()
{
    grep -v '^Old'
}

title ''
${CTAGS} --quiet --options=NONE --list-roles= | ignore_xml | ignore_old | ignore_yaml

title 'all.*'
${CTAGS} --quiet --options=NONE --list-roles='all.*' | ignore_xml | ignore_old | ignore_yaml

title 'C.*'
${CTAGS} --quiet --options=NONE --list-roles='C.*'

title 'all.d'
${CTAGS} --quiet --options=NONE --list-roles='all.d' | ignore_xml | ignore_old | ignore_yaml

title 'Sh.s'
${CTAGS} --quiet --options=NONE --list-roles='Sh.s'

# --roles-all=
title 'C.* with disabling  all roles of all languages'
${CTAGS} --quiet --options=NONE --roles-all= --list-roles='C.*'

# --roles-all.*=
title 'C.* with disabling all roles of all kinds of all languages'
${CTAGS} --quiet --options=NONE --roles-all.'*'= --list-roles='C.*'

# --roles-all.*='*'
title 'C.* with enabling all roles of all kinds in all languages' \
	  'after disabling system role of header kind of C language'
${CTAGS} --quiet --options=NONE --roles-C.h=-'{system}' \
		 --roles-all.'*'='*' --list-roles='C.*'

#--roles-all='*'
title 'C.* with enabling all roles in all languages' \
	  'after disabling system role of header kind of C language'
${CTAGS} --quiet --options=NONE --roles-C.h=-'{system}' \
		 --roles-all='*' --list-roles='C.*'

# --roles-<LANG>=
title 'C.* with disabling all roles in C language'
${CTAGS} --quiet --options=NONE --roles-C= --list-roles='C.*'
title 'Sh.* with disabling all roles in C language'
${CTAGS} --quiet --options=NONE --roles-C= --list-roles='Sh.*'

# --roles-<LANG>.*=
title 'C.* with disabling all roles of all kinds in C language'
${CTAGS} --quiet --options=NONE --roles-C.'*'= --list-roles='C.*'
title 'Sh.* with disabling all roles of all kinds in C language'
${CTAGS} --quiet --options=NONE --roles-C.'*'= --list-roles='Sh.*'

# --roles-<LANG>='*'
title 'C.* with enabling all roles in C language' \
	  'after disabling all roles in all languages'
${CTAGS} --quiet --options=NONE --roles-all= --roles-C='*' --list-roles='C.*'
title 'Sh.* with enabling all roles in C language' \
	  'after disabling all roles in all languages'
${CTAGS} --quiet --options=NONE --roles-all= --roles-C='*' --list-roles='Sh.*'

# --roles-<LANG>.*='*'
title 'C.* with enabling all roles of all kinds in C language' \
	  'after disabling all roles in all languages'
${CTAGS} --quiet --options=NONE --roles-all= --roles-C.'*'='*' --list-roles='C.*'
title 'Sh.* with enabling all roles of all kinds in C language' \
	  'after disabling all roles in all languages'
${CTAGS} --quiet --options=NONE --roles-all= --roles-C.'*'='*' --list-roles='Sh.*'

# --roles-<LANG>.{kind}=
title 'C.* with disabling all roles of {header} kind in C language'
${CTAGS} --quiet --options=NONE --roles-C.'{header}'= --list-roles='C.*'
title 'Sh.* with disabling all roles of {header} kind in C language'
${CTAGS} --quiet --options=NONE --roles-C.'{header}'= --list-roles='Sh.*'

# --roles-<LANG>.k=
title 'C.* with disabling all roles of h kind in C language'
${CTAGS} --quiet --options=NONE --roles-C.h= --list-roles='C.*'
title 'Sh.* with disabling all roles of h kind in C language'
${CTAGS} --quiet --options=NONE --roles-C.h= --list-roles='Sh.*'

# --roles-<LANG>.{kind}=*
title 'C.* with enabling all roles of {header} kind in C language' \
	  'after disabling all roles in all languages'
${CTAGS} --quiet --options=NONE --roles-all= --roles-C.'{header}'='*' --list-roles='C.*'
title 'Sh.* with enabling all roles of {header} kind in C language' \
	  'after disabling all roles in all languages'
${CTAGS} --quiet --options=NONE --roles-all= --roles-C.'{header}'='*' --list-roles='Sh.*'

# --roles-<LANG>.k=*
title 'C.* with enabling all roles of h kind in C language' \
	  'after disabling all roles in all languages'
${CTAGS} --quiet --options=NONE --roles-all= --roles-C.'h'='*' --list-roles='C.*'
title 'Sh.* with enabling all roles of h kind in C language' \
	  'after disabling all roles in all languages'
${CTAGS} --quiet --options=NONE --roles-all= --roles-C.'h'='*' --list-roles='Sh.*'

# --roles-<LANG>.{kind}=[+|-]{role}
# --roles-<LANG>.k=[+|-]{role}
title 'C.* with disabling system role of h kind'
${CTAGS} --quiet --options=NONE --roles-C.h='-{system}' --list-roles='C.*'
title 'C.* with disabling system role of {header} kind'
${CTAGS} --quiet --options=NONE --roles-C.'{header}'='-{system}' --list-roles='C.*'

title 'C.* with enabling system role of h kind after disabling the role'
${CTAGS} --quiet --options=NONE --roles-C.h='-{system}+{system}' --list-roles='C.*'
title 'C.* with enabling system role of {header} kind after disabling the role'
${CTAGS} --quiet --options=NONE --roles-C.'{header}'='-{system}+{system}' --list-roles='C.*'

title 'C.* with disabling system and local roles of h kind'
${CTAGS} --quiet --options=NONE --roles-C.h='-{system}{local}' --list-roles='C.*'
title 'C.* with disabling system and local roles of {header} kind'
${CTAGS} --quiet --options=NONE --roles-C.'{header}'='-{system}{local}' --list-roles='C.*'

title 'C.* with enabling system and local roles of h kind' \
	  'after disabling all roles in all languages'
${CTAGS} --quiet --options=NONE --roles-all= --roles-C.h='+{system}{local}' --list-roles='C.*'
title 'C.* with enabling system and local roles of {header} kind' \
	  'after disabling all roles in all languages'
${CTAGS} --quiet --options=NONE --roles-all= --roles-C.'{header}'='+{system}{local}' --list-roles='C.*'

title 'C.* with disabling local role of h kind and undef role of d kind'
${CTAGS} --quiet --options=NONE --roles-C.h='-{local}' --roles-C.d='-{undef}' --list-roles='C.*'

# miscellaneous combinations
title 'C.* with enabling all roles of header kinds after disabling all roles of the kind'
${CTAGS} --quiet --options=NONE --roles-C.'{header}'= --roles-C.'{header}'='*' --list-roles='C.*'

title 'C.* with enabling all roles of header kinds after disabling all roles of the kinds of C language'
${CTAGS} --quiet --options=NONE --roles-C.'*'= --roles-C.'{header}'='*' --list-roles='C.*'

title 'C.* with enabling all roles of header kinds after disabling all roles of the kinds of C language (short notation)'
${CTAGS} --quiet --options=NONE --roles-C= --roles-C.'{header}'='*' --list-roles='C.*'
