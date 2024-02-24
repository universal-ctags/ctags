# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE --kinds-C++=+x --fields=+Ere-T --extras=+qrf -o - input.cpp \
    | sed -e 's|[^	]*\(input.cpp\)|\1|'
