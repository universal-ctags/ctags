# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

echo '{header}'
${CTAGS} --list-roles=all.'{header}'

echo '{header}I'
${CTAGS} --list-roles=all.'{header}I'

echo 'd{header}'
${CTAGS} --list-roles=all.'d{header}'
