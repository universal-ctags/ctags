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

echo '#e-ctags output'
$CTAGS --quiet --options=NONE --output-format=e-ctags -o -           'src\input.c'
$CTAGS --quiet --options=NONE --output-format=e-ctags -o - $OPT      'src\input.c'
$CTAGS --quiet --options=NONE --output-format=e-ctags -o - ${OPT}=no 'src\input.c'

echo '#xref output'
$CTAGS --quiet --options=NONE --output-format=xref -o -           'src\input.c'
$CTAGS --quiet --options=NONE --output-format=xref -o - $OPT      'src\input.c'
$CTAGS --quiet --options=NONE --output-format=xref -o - ${OPT}=no 'src\input.c'

# TODO: json output
