UNICODE_VERSION = 9.0.0

UCD_FILES = \
	    $(UNICODE_VERSION)/Blocks.txt \
	    $(UNICODE_VERSION)/CaseFolding.txt \
	    $(UNICODE_VERSION)/DerivedAge.txt \
	    $(UNICODE_VERSION)/DerivedCoreProperties.txt \
	    $(UNICODE_VERSION)/PropertyAliases.txt \
	    $(UNICODE_VERSION)/PropertyValueAliases.txt \
	    $(UNICODE_VERSION)/PropList.txt \
	    $(UNICODE_VERSION)/Scripts.txt \
	    $(UNICODE_VERSION)/UnicodeData.txt \
	    $(UNICODE_VERSION)/GraphemeBreakProperty.txt

update: casefold.h name2ctype.h
	cp casefold.h name2ctype.h ../enc/unicode

download:
	./download-ucd.sh $(UNICODE_VERSION)


casefold.h: CaseFolding.txt UnicodeData.txt SpecialCasing.txt
	$(RUBY) ./case-folding.rb -m . -o casefold.h

#name2ctype.h: name2ctype.kwd
#	./convert-name2ctype.sh name2ctype.kwd > name2ctype.h

name2ctype.h: $(UCD_FILES)
	$(RUBY) ./enc-unicode.rb --header $(UNICODE_VERSION) > name2ctype.h


clean:
	-rm -f casefold.h name2ctype.kwd name2ctype.h
	-rm -f $(UCD_FILES)
	-rmdir $(UNICODE_VERSION)
