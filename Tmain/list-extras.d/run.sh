# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

ignore_pcre2()
{
	grep -v XS
}

${CTAGS} --quiet --options=NONE --extras='*' --with-list-header --list-extras | ignore_pcre2
${CTAGS} --quiet --options=NONE --extras='*' --with-list-header --machinable --list-extras | ignore_pcre2
${CTAGS} --quiet --options=NONE --extras= --with-list-header --list-extras=NONE | ignore_pcre2
