# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --fields=+Er --extra=+qr. -o - input.cpp | sed -e 's|[^	]*\(input.cpp\)|\1|'

