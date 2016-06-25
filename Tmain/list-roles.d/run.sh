# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

title()
{
    echo
    echo '#'
    echo '#' $1
    echo '#'
}

ignore_xml()
{
    grep -v 'Glade\|Ant\|Maven2\|XSLT'
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
${CTAGS} --quiet --options=NONE --_list-roles= | ignore_xml | ignore_old

title 'all:*'
${CTAGS} --quiet --options=NONE --_list-roles='all:*' | ignore_xml | ignore_old

title 'C:*'
${CTAGS} --quiet --options=NONE --_list-roles='C:*'

title 'all:h'
${CTAGS} --quiet --options=NONE --_list-roles='all:h' | ignore_xml | ignore_old

title 'Sh:s'
${CTAGS} --quiet --options=NONE --_list-roles='Sh:s'
