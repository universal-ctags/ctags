# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

run_with_format()
{
    echo '#' $1
    ${CTAGS} --quiet --options=NONE --output-format=$1 -o - input.c
}


run_with_format ctags
run_with_format etags
run_with_format xref
