# Copyright: 2016 Masatake YAMATO
# License: GPL-2

CTAGS=$1

$CTAGS --quiet --options=NONE -o - \
	   --langdef=XYZ --regex-XYZ='/aaaaaaaaaaaaaaaaaaaaa/\0/x/' \
	   --language-force=XYZ --pseudo-tags='TAG_KIND_DESCRIPTION' --extras=p \
	   run.sh
