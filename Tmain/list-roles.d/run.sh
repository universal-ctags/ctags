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
