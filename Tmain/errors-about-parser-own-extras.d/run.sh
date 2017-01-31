# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1"
O="--quiet --options=NONE --with-list-header=no"

echo '#lang' 1>&2
${CTAGS} ${O} --extras-NOSUCHLANG=-'{whitespaceSwapped}' --list-extras
${CTAGS} ${O} --extras-NOSUCHLANG=-'{whitespaceSwapped}' --list-extras=
${CTAGS} ${O} --extras-Robot=-'{whitespaceSwapped}' --list-extras=NOSUCHLANG

echo '#extras' 1>&2
${CTAGS} ${O} --extras-Robot=-'{NOSUCHEXTRA}' --list-extras
${CTAGS} ${O} --extras-Robot=-'{NOSUCHEXTRA}' --list-extras=
${CTAGS} ${O} --extras-Robot=-'{NOSUCHEXTRA}' --list-extras=NOSUCHLANG

echo '#null' 1>&2
${CTAGS} ${O} --extras-= --list-extras

# not an error, just set all extras to false.
${CTAGS} ${O} --extras-Robot= --list-extras

${CTAGS} ${O} --extras-Robot=-'{whitespaceSwapped}' --list-extras=

echo '#null null' 1>&2

# About --extras-Robot=, not an error, just set all extras to false.
${CTAGS} ${O} --extras-Robot= --list-extras=
${CTAGS} ${O} --extras-= --list-extras=
