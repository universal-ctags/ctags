# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

{
{
echo '# echo unknown lang'
${CTAGS} --extradef-NOSUCHLANG
${CTAGS} --extradef-NOSUCHLANG=extra,desc

echo '# no option value'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=

echo '# wrong char in a field name'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=:
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=:abc
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=:abc,
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=:abc,description

echo '# empty extra name'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=,
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=,abc
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=,abc,
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=,abc,description

echo '# empty description'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=abc
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=abc,

echo '# no input file'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY=abc,desc

echo '# inject a flag separator'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY='extra,desc{foo}' --list-extras=IMAGINARY 2>&1

echo '# inject a broken flag separator(1)'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY='extra,desc{foo' --list-extras=IMAGINARY 2>&1

echo '# inject a broken flag separator(2)'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY='extra,desc{' --list-extras=IMAGINARY 2>&1

echo '# use a { in description (1)'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY='extra,desc\{' --list-extras=IMAGINARY 2>&1

echo '# use a { in description (2)'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY='extra,desc\{}' --list-extras=IMAGINARY 2>&1

echo '# use a \ in description'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY='extra,desc\\backslash' --list-extras=IMAGINARY 2>&1

echo '# description started from {'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY='extra,{' --list-extras=IMAGINARY 2>&1

echo '# description started from \{'
${CTAGS} --langdef=IMAGINARY --extradef-IMAGINARY='extra,\{' --list-extras=IMAGINARY 2>&1

} 2>&1
} | sed -e 's/\.exe//g'
