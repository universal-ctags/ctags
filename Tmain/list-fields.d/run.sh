# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

with_field()
{
    local field=$1
    local lang=$2
    : &&
	echo "#$field" &&
	$CTAGS --quiet --options=NONE $3 --machinable --fields=$field -o - input.$lang
}

ignore_xpath ()
{
    grep -v Maven2 | grep -v XML
}

: &&
    $CTAGS --quiet --options=NONE --machinable --with-list-header --list-fields  \
	| ignore_xpath &&
    with_field "" java &&
    with_field a  java &&
    with_field i  java &&
    with_field kz java &&
    with_field Kz java &&
    with_field k  java &&
    with_field K  java &&
    with_field l  java &&
    with_field m  java &&
    with_field n  java &&
    with_field s  java &&
    with_field sZ java &&
    with_field f  c &&
    with_field S  c &&
    with_field t  c &&
    with_field r  sh --extras=+r &&
    with_field r  sh "--extras=+r -x --_xformat=%R/%r"
