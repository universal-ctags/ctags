# Copyright: 2022 Masatake YAMATO
# License: GPL-2

CTAGS=$1

for f in input-firstline0.unknown \
			 input-firstline1.unknown \
			 input-firstline2.unknown \
			 input-firstline3.unknown \
			 input-firstline4.unknown \
			 input-firstline5.unknown \
			 input-lastlist0.unknown  \
			 input-lastlist1.unknown  \
			 input-lastlist2.unknown  \
		 ; do
	$CTAGS --quiet --options=NONE -G --print-language $f
done
