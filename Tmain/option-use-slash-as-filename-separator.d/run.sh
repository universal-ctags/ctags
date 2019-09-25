# Copyright: 2019 Masatake YAMATO
# License: GPL-2

CTAGS=$1
OPT=--use-slash-as-filename-separator

. ../utils.sh

exit_unless_win32 $CTAGS

echo '#u-ctags output'
$CTAGS --quiet --options=NONE -o -      'src\input.c'
$CTAGS --quiet --options=NONE -o - $OPT 'src\input.c'
$CTAGS --quiet --options=NONE -o - ${OPT}=no 'src\input.c'

echo '#u-ctags ptag output'
{
$CTAGS --quiet --options=NONE --extras=p -o -      'src\input.c'
$CTAGS --quiet --options=NONE --extras=p -o - $OPT 'src\input.c'
$CTAGS --quiet --options=NONE --extras=p -o - ${OPT}=no 'src\input.c'
} | grep TAG_OUTPUT_FILESEP

echo '#e-ctags output'
$CTAGS --quiet --options=NONE --output-format=e-ctags -o -           'src\input.c'
$CTAGS --quiet --options=NONE --output-format=e-ctags -o - $OPT      'src\input.c'
$CTAGS --quiet --options=NONE --output-format=e-ctags -o - ${OPT}=no 'src\input.c'

echo '#e-ctags ptag output'
{
$CTAGS --quiet --options=NONE --extras=p --output-format=e-ctags -o -           'src\input.c'
$CTAGS --quiet --options=NONE --extras=p --output-format=e-ctags -o - $OPT      'src\input.c'
$CTAGS --quiet --options=NONE --extras=p --output-format=e-ctags -o - ${OPT}=no 'src\input.c'
} | grep TAG_OUTPUT_FILESEP

echo '#xref output'
$CTAGS --quiet --options=NONE --output-format=xref -o -           'src\input.c'
$CTAGS --quiet --options=NONE --output-format=xref -o - $OPT      'src\input.c'
$CTAGS --quiet --options=NONE --output-format=xref -o - ${OPT}=no 'src\input.c'

# TODO: json output
