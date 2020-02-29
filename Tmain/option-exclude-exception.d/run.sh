# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS="$1"

${CTAGS} --quiet --options=NONE -o - -R --exclude='*/bazel-*' \
		 --exclude-exception='*/bazel-x/*' \
		 --exclude-exception='*/bazel-z/*' \
		 input.d
