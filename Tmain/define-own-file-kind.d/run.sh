CTAGS=$1

${CTAGS} --quiet --options=NONE --langdef=foo'{fileKind=c}' --list-file-kind | grep ^foo
