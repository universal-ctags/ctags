# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

ignore_pcre2()
{
	grep -v 'XS'
}

echo '# resetting'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras='{subparser}' --list-extras | ignore_pcre2

echo '# enabling 1'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=+'{pseudo}' --list-extras | ignore_pcre2

echo '# disabling 1'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=-'{fileScope}' --list-extras | ignore_pcre2

echo '# combination'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=-'{fileScope}+{inputFile}{reference}' --list-extras | ignore_pcre2

echo '# combination with letters'
${CTAGS} --quiet --options=NONE --with-list-header=no \
	 --extras=-'{fileScope}+p{inputFile}q{reference}-f' --list-extras | ignore_pcre2
