# Copyright: 2016 Masatake YAMATO
# License: GPL-2
CTAGS=$1

. ../utils.sh

if ! sort --help | grep --quiet GNU; then
    skip "GNU sort is needed to run this test case"
fi

list_languages()
{
    ${CTAGS} --quiet --options=NONE --list-languages
}

list_languages > ./ll.tmp
list_languages | sort --ignore-case > ./sorted-ll.tmp
diff -uN ./ll.tmp ./sorted-ll.tmp
r=$?
rm ./ll.tmp ./sorted-ll.tmp
exit $r
