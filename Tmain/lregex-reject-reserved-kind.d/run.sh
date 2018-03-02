# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE -o - --langdef=foo --regex-foo=/a/\0/F/ run.sh ||
${CTAGS} --quiet --options=NONE -o - --langdef=foo --regex-foo=/a/\0/x,file/ run.sh
