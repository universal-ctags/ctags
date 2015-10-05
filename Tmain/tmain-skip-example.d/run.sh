# Copyright: 2015 Masatake YAMATO
# License: GPL-2

if ${CTAGS} --list-features | grep -q afasdfasfasfsa; then
    echo
else
    echo "example: no such feature"
    exit 77
fi
