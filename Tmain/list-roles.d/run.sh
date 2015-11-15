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

title ''
${CTAGS} --quiet --options=NONE --_list-roles=

title 'all:*'
${CTAGS} --quiet --options=NONE --_list-roles='all:*'

title 'C:*'
${CTAGS} --quiet --options=NONE --_list-roles='C:*'

title 'all:h'
${CTAGS} --quiet --options=NONE --_list-roles='all:h'

title 'Sh:s'
${CTAGS} --quiet --options=NONE --_list-roles='Sh:s'
