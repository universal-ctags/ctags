# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

echo '{header}'
${CTAGS} --list-roles=all.'{header}'

echo '{header}I'
${CTAGS} --machinable=yes --list-roles=all.'{header}I' | grep -v Texinfo

echo 'd{header}'
${CTAGS} --list-roles=all.'d{header}'
