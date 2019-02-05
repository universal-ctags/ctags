# -*- makefile -*-
include makefiles/optlib2c_input.mak
.PHONY: list-optlib2c-input
list-optlib2c-input:
	@for x in $(OPTLIB2C_INPUT); do echo $$x; done
