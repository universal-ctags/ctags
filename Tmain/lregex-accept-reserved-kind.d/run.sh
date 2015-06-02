CTAGS=$1

${CTAGS} --quiet --options=NONE -o - --langdef=foo'{fileKind=!}' --regex-foo=/a/\0/F/ run.sh
