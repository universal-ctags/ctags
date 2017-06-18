# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

echo '{header}'
${CTAGS} --_list-roles=all.'{header}'

echo '{header}I'
${CTAGS} --_list-roles=all.'{header}I'

echo 'd{header}'
${CTAGS} --_list-roles=all.'d{header}'
