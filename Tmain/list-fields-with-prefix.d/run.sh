# Copyright: 2015 Masatake YAMATO
# License: GPL-2
CTAGS=$1

ignore_xpath ()
{
    grep -v Maven2 | grep -v XML
}

$CTAGS --quiet --options=NONE --put-field-prefix --list-fields \
    | grep UCTAGS \
    | ignore_xpath
