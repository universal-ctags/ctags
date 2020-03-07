# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1
O="--quiet --options=NONE "

${CTAGS} $O \
		 --extras=p --pseudo-tags=TAG_EXTRA_DESCRIPTION \
		 --extras=+g \
		 --extras-Robot='{whitespaceSwapped}' \
		 -o - \
		 input.robot

${CTAGS} $O \
		 --extras=p --pseudo-tags=TAG_EXTRA_DESCRIPTION \
		 --extras-Robot=-'{whitespaceSwapped}' \
		 -o - \
		 input.robot
