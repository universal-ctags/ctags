# Copyright: 2020 Masatake YAMATO
# License: GPL-2

CTAGS=$1
O="--quiet --options=NONE "

echo '# single with no curly bracket'
${CTAGS} $O \
		 --extras=+p --pseudo-tags=TAG_PROGRAM_URL \
		 -o - \
		 input.c

echo '# single with curly bracket'
${CTAGS} $O \
		 --extras=+p --pseudo-tags='{TAG_PROGRAM_URL}' \
		 -o - \
		 input.c

echo '# single with + no curly bracket'
${CTAGS} $O \
		 --extras=+p --pseudo-tags= \
		 --pseudo-tags=+TAG_PROGRAM_URL \
		 -o - \
		 input.c

echo '# single with + curly bracket'
${CTAGS} $O \
		 --extras=+p --pseudo-tags= \
		 --pseudo-tags=+'{TAG_PROGRAM_URL}' \
		 -o - \
		 input.c

echo '# single with +- no curly bracket'
${CTAGS} $O \
		 --extras=+p --pseudo-tags= \
		 --pseudo-tags=+TAG_PROGRAM_VERSION \
		 --pseudo-tags=+TAG_PROGRAM_URL \
		 --pseudo-tags=-TAG_PROGRAM_VERSION \
		 -o - \
		 input.c

echo '# single with +- curly bracket'
${CTAGS} $O \
		 --extras=+p --pseudo-tags= \
		 --pseudo-tags=+'{TAG_PROGRAM_VERSION}' \
		 --pseudo-tags=+'{TAG_PROGRAM_URL}' \
		 --pseudo-tags=-'{TAG_PROGRAM_VERSION}' \
		 -o - \
		 input.c

echo '# multiple specifications with +- curly bracket'
${CTAGS} $O \
		 --extras=+p --pseudo-tags= \
		 --pseudo-tags='+{TAG_PROGRAM_VERSION}{TAG_FILE_SORTED}{TAG_PROGRAM_URL}-{TAG_PROGRAM_VERSION}' \
		 -o - \
		 input.c

echo '# multiple specifications with -+- curly bracket'
${CTAGS} $O \
		 --extras=+p --pseudo-tags= \
		 --pseudo-tags='-{TAG_PROGRAM_VERSION}+{TAG_PROGRAM_VERSION}{TAG_FILE_FORMAT}{TAG_PROGRAM_URL}-{TAG_PROGRAM_VERSION}' \
		 -o - \
		 input.c
