# Copyright: 2018 Masatake YAMATO
# License: GPL-2

CTAGS="$1 --quiet --options=NONE"

/bin/echo param: '|(.)|\1|^ => ignoring'
${CTAGS} --langdef=x --regex-x='|(.)|\1|^' --list-kinds-full=x
# In this case, ctags ignores the substring after the last '|'.
# It is evaluated as flags.

/bin/echo param: '|(.)|\1|^| => warning'
${CTAGS} --langdef=x --regex-x='|(.)|\1|^|' --list-kinds-full=x
# In this case, ctags warns specifying a wrong kind letter '^'.

/bin/echo param: '|(.)|\1|, => ignoring'
${CTAGS} --langdef=x --regex-x='|(.)|\1|,' --list-kinds-full=x
# In this case, ctags ignores the substring after the last '|'.
# It is evaluated as flags.

/bin/echo param: '|(.)|\1|,| => using the default letter and name'
${CTAGS} --langdef=x --regex-x='|(.)|\1|,|' --list-kinds-full=x
# In this case, ctags recognizes a kind letter and name
# are not given; 'r' and "regex" are used as default values.
