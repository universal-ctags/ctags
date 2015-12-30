# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

convsep ()
{
    sed -e 's#\\\\#/#g'
}

echo '# DEPTH=1'
${CTAGS} --quiet --options=NONE --maxdepth=1 -R -o - ./src | convsep
echo '# DEPTH=2'
${CTAGS} --quiet --options=NONE --maxdepth=2 -R -o - ./src | convsep
exit $?
