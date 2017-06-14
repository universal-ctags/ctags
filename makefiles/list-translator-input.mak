# -*- makefile -*-
include makefiles/translator_input.mak
.PHONY: list-translator-input
list-translator-input:
	@for x in $(TRANSLATOR_INPUT); do echo $$x; done
