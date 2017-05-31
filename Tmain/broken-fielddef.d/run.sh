# Copyright: 2017 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

{
{
echo '# echo unknown lang'
${CTAGS} --_fielddef-NOSUCHLANG --_quit
${CTAGS} --_fielddef-NOSUCHLANG=field,desc --_quit

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

} 2>&1
} | sed -e 's/\.exe//g'
