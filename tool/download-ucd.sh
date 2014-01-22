#!/bin/sh

files='Blocks.txt CaseFolding.txt DerivedAge.txt DerivedCoreProperties.txt PropertyAliases.txt PropertyValueAliases.txt PropList.txt Scripts.txt UnicodeData.txt'

# remove old files
rm $files

for i in $files; do
    if [ -z $1 ]; then
	# latest version
	echo http://www.unicode.org/Public/UNIDATA/$i
    else
	# specific version
	echo http://www.unicode.org/Public/$1/ucd/$i
    fi
done | xargs wget
