#	$Id$
#
#	Copyright (c) 1996-2001, Darren Hiebert
#
#	Development makefile for Exuberant Ctags, used to build releases.
#	Requires GNU make.

CTAGS_TEST = ctags
CTAGS_REF = ctags.ref
TEST_OPTIONS = -nu --c-types=+px

DIFF_OPTIONS = -I '^!_TAG'
DIFF = if diff $(DIFF_OPTIONS) tags.ref tags.test > $(DIFF_FILE); then \
		rm -f tags.ref tags.test $(DIFF_FILE) ; \
		echo "Passed" ; \
	  else \
		echo "FAILED: differences left in $(DIFF_FILE)" ; \
	  fi

.PHONY: test test.include test.fields test.eiffel test.linux

test: test.include test.fields test.linedir test.eiffel test.linux

test.%: DIFF_FILE = $@.diff

REF_INCLUDE_OPTIONS = $(TEST_OPTIONS) --format=1
TEST_INCLUDE_OPTIONS = $(REF_INCLUDE_OPTIONS) --format=1
test.include: $(CTAGS_TEST) $(CTAGS_REF)
	@ echo -n "Testing tag inclusion..."
	@ $(CTAGS_REF) -R $(REF_INCLUDE_OPTIONS) -o tags.ref Test
	@ $(CTAGS_TEST) -R $(TEST_INCLUDE_OPTIONS) -o tags.test Test
	@- $(DIFF)

REF_FIELD_OPTIONS = $(TEST_OPTIONS) --fields=+afmikKlnsz
TEST_FIELD_OPTIONS = $(TEST_OPTIONS) --fields=+afmikKlnsz
test.fields: $(CTAGS_TEST) $(CTAGS_REF)
	@ echo -n "Testing extension fields..."
	@ $(CTAGS_REF) -R $(REF_FIELD_OPTIONS) -o tags.ref Test
	@ $(CTAGS_TEST) -R $(TEST_FIELD_OPTIONS) -o tags.test Test
	@- $(DIFF)

REF_LINEDIR_OPTIONS = $(TEST_OPTIONS) --line-directives -n
TEST_LINEDIR_OPTIONS = $(TEST_OPTIONS) --line-directives -n
test.linedir: $(CTAGS_TEST) $(CTAGS_REF)
	@ echo -n "Testing line directives..."
	@ $(CTAGS_REF) $(REF_LINEDIR_OPTIONS) -o tags.ref Test/line_directives.c
	@ $(CTAGS_TEST) $(TEST_LINEDIR_OPTIONS) -o tags.test Test/line_directives.c
	@- $(DIFF)

REF_EIFFEL_OPTIONS = $(TEST_OPTIONS) --format=1 --languages=eiffel
TEST_EIFFEL_OPTIONS = $(TEST_OPTIONS) --format=1 --languages=eiffel
EIFFEL_DIRECTORY = $(ISE_EIFFEL)/library
test.eiffel: $(CTAGS_TEST) $(CTAGS_REF)
	@ echo -n "Testing Eiffel tag inclusion..."
	@ $(CTAGS_REF) -R $(REF_EIFFEL_OPTIONS) -o tags.ref $(EIFFEL_DIRECTORY)
	@ $(CTAGS_TEST) -R $(TEST_EIFFEL_OPTIONS) -o tags.test $(EIFFEL_DIRECTORY)
	@- $(DIFF)

REF_LINUX_OPTIONS = $(TEST_OPTIONS) --fields=k
TEST_LINUX_OPTIONS = $(TEST_OPTIONS) --fields=k
LINUX_DIRECTORY = /usr/src/linux-2.4
test.linux: $(CTAGS_TEST) $(CTAGS_REF)
	@ echo -n "Testing Linux tag inclusion..."
	@ $(CTAGS_REF) -R $(REF_LINUX_OPTIONS) -o tags.ref $(LINUX_DIRECTORY)
	@ $(CTAGS_TEST) -R $(TEST_LINUX_OPTIONS) -o tags.test $(LINUX_DIRECTORY)
	@- $(DIFF)

TEST_ARTIFACTS = test.*.diff tags.ref tags.test

# vi:ts=4 sw=4
