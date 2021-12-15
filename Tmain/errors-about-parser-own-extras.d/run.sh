CTAGS="$1"
O="--quiet --options=NONE --with-list-header=no"
${CTAGS} ${O} --extras-NOSUCHLANG=-'{whitespaceSwapped}' --list-extras
