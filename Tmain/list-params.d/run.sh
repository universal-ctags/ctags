# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1
C="${CTAGS} --quiet --options=NONE"

ignore_pcre2()
{
	grep -v Texinfo
}

{
echo '# ALL'
${C} --with-list-header=yes --list-params
echo

echo '# ALL MACHINABLE'
${C} --with-list-header=yes --machinable --list-params
echo

echo '# ALL MACHINABLE NOHEADER'
${C} --with-list-header=no  --machinable --list-params
echo

echo '# CPP'
${C} --list-params=CPreProcessor
echo

echo '# CPP MACHINABLE'
${C} --with-list-header=yes --machinable --list-params=CPreProcessor
echo

echo '# CPP MACHINABLE NOHEADER'
${C} --with-list-header=no  --machinable --list-params=CPreProcessor
echo

echo '# CPP MACHINABLE NOHEADER + PARAM DEFINE WITH CMDLINE'
${C} --_paramdef-CPreProcessor='pragma,handle program' --with-list-header=no  --machinable --list-params=CPreProcessor
echo
} | ignore_pcre2
