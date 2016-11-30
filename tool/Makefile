update: casefold.h name2ctype.h
	cp casefold.h name2ctype.h ../enc/unicode

download:
	./download-ucd.sh


casefold.h: CaseFolding.txt UnicodeData.txt SpecialCasing.txt
	$(RUBY) ./case-folding.rb -m . -o casefold.h

name2ctype.h: name2ctype.kwd
	./convert-name2ctype.sh name2ctype.kwd > name2ctype.h

name2ctype.kwd: Blocks.txt CaseFolding.txt DerivedAge.txt DerivedCoreProperties.txt PropertyAliases.txt PropertyValueAliases.txt PropList.txt Scripts.txt UnicodeData.txt
	$(RUBY) ./enc-unicode.rb . > name2ctype.kwd


clean:
	rm -f casefold.h name2ctype.kwd name2ctype.h
	rm -f Blocks.txt CaseFolding.txt DerivedAge.txt DerivedCoreProperties.txt PropertyAliases.txt PropertyValueAliases.txt PropList.txt Scripts.txt SpecialCasing.txt UnicodeData.txt
