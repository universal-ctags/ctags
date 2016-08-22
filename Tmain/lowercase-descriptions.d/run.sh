# Copyright: 2016 Doug Kearns
# License: GPL-2

# PURPOSE: descriptions in --list-* output should generally be lowercase but
# may be incidentally capitalised if they start with a proper name or acronym.

CTAGS=$1

is_capitalised_description()
{
    awk --field-separator='\t' '$NF ~ /^[[:upper:]]/'
}

echo '# --list-extra'
${CTAGS} --quiet --options=NONE --machinable --with-list-header=no \
    --list-extra | is_capitalised_description

echo '# --list-fields'
${CTAGS} --quiet --options=NONE --machinable --with-list-header=no \
    --list-fields | is_capitalised_description

echo '# --list-kinds-full'
${CTAGS} --quiet --options=NONE --machinable --with-list-header=no \
    --list-kinds-full | is_capitalised_description

echo '# --list-pseudo-tags'
${CTAGS} --quiet --options=NONE --with-list-header=no \
    --list-pseudo-tags | is_capitalised_description
