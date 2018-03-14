# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

{
{
echo '# echo unknown lang'
${CTAGS} --_fielddef-NOSUCHLANG
${CTAGS} --_fielddef-NOSUCHLANG=field,desc

echo '# no option value'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=

echo '# wrong char in a field name'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=:
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=:abc
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=:abc,
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=:abc,description

echo '# empty field name'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=,
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=,abc
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=,abc,
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=,abc,description

echo '# empty description'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=abc
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=abc,

echo '# no input file'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY=abc,desc

echo '# inject a flag separator'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc{foo}' --list-fields=IMAGINARY

echo '# inject a broken flag separator(1)'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc{foo' --list-fields=IMAGINARY

echo '# inject a broken flag separator(2)'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc{' --list-fields=IMAGINARY

echo '# use a { in description (1)'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc\{' --list-fields=IMAGINARY

echo '# use a { in description (2)'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc\{}' --list-fields=IMAGINARY

echo '# use a \ in description'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,desc\\backslash' --list-fields=IMAGINARY

echo '# description started from {'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,{' --list-fields=IMAGINARY

echo '# description started from \{'
${CTAGS} --langdef=IMAGINARY --_fielddef-IMAGINARY='field,\{' --list-fields=IMAGINARY

} 2>&1
} | sed -e 's/\.exe//g'
