# -*- makefile -*-
include makefiles/txt2cstr_input.mak
.PHONY: list-txt2cstr-input
list-txt2cstr-input:
	@for x in $(TXT2CSTR_INPUT); do echo $$x; done
