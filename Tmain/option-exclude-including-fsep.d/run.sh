# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS="$1"

${CTAGS} --quiet --options=NONE -o - -R --exclude='*/bazel-*' \
		 input.d
