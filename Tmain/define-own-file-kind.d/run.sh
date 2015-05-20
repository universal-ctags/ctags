CTAGS=$1

${CTAGS} --quiet --langdef=foo'{fileKind=c}' --list-file-kind | grep ^foo
