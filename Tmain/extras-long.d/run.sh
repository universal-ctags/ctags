# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

no_yaml()
{
	grep -v I18nRubyGem
}

echo '# resetting'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras='{subparser}' --list-extras | no_yaml

echo '# enabling 1'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=+'{pseudo}' --list-extras | no_yaml

echo '# disabling 1'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=-'{fileScope}' --list-extras | no_yaml

echo '# combination'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=-'{fileScope}+{inputFile}{reference}' --list-extras | no_yaml

echo '# combination with letters'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=-'{fileScope}+p{inputFile}q{reference}-f' --list-extras | no_yaml
