# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

${CTAGS} --quiet --options=NONE -o - \
	 --extras=p --kinds-all= \
	 --pseudo-tags=TAG_PROGRAM_NAME \
	 input.php

${CTAGS} --quiet --options=NONE -o - \
	 --extras=+p --kinds-all= \
	 --pseudo-tags=+TAG_KIND_SEPARATOR \
	 --pseudo-tags=-TAG_PROGRAM_VERSION \
	 --pseudo-tags=-TAG_PROC_CWD \
	 input.php
