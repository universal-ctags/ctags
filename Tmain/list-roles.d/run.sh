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
    grep -v 'Glade\|Ant\|Maven2'
}

title ''
${CTAGS} --quiet --options=NONE --_list-roles= | ignore_xml

title 'all:*'
${CTAGS} --quiet --options=NONE --_list-roles='all:*' | ignore_xml

title 'C:*'
${CTAGS} --quiet --options=NONE --_list-roles='C:*'

title 'all:h'
${CTAGS} --quiet --options=NONE --_list-roles='all:h' | ignore_xml

title 'Sh:s'
${CTAGS} --quiet --options=NONE --_list-roles='Sh:s'
