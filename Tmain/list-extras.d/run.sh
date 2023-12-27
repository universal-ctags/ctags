# Copyright: 2015 Masatake YAMATO
# License: GPL-2

CTAGS=$1

no_yaml()
{
	grep -v I18nRubyGem
}

${CTAGS} --quiet --options=NONE --extras='*' --with-list-header --list-extras | no_yaml
${CTAGS} --quiet --options=NONE --extras='*' --with-list-header --machinable --list-extras | no_yaml
${CTAGS} --quiet --options=NONE --extras= --with-list-header --list-extras=NONE | no_yaml
