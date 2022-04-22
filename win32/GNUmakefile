#
# Copyright (c) 2021, Hiroo Hayashi
#
# This source code is released for free distribution under the terms of the
# GNU General Public License version 2 or (at your option) any later version.
#
# Makefile to generate ctags_vs2013.vcxproj and ctags_vs2013.vcxproj.filters
#
#   usage: make [-B]
#
# Restrictions:
#   - Input Files: ctags_vs2013.vcxproj.in and ctags_vs2013.vcxproj.filters.in
#     - The last charactor of the files must be '>'.
#       cf. check_eof_chars_in_vcxproj() in misc/src-check.
#     - Other lines must be end with LF.
#   - GNU make is required.

VCXPROJ = ctags_vs2013.vcxproj
VCXPROJ_FILTERS = ctags_vs2013.vcxproj.filters
SOURCE_MAK = ../source.mak

all: $(VCXPROJ) $(VCXPROJ_FILTERS)

include $(SOURCE_MAK)

# exclude some files for Win32 and replace a slash (/) to a backslash (\)
MVC_SRCS = $(MVC_GNULIB_SRCS) $(CMDLINE_SRCS) $(LIB_SRCS) $(OPTLIB2C_SRCS) $(PARSER_SRCS) $(OPTSCRIPT_DSL_SRCS) $(DEBUG_SRCS) $(WIN32_SRCS)
MVC_SRCS_EXCLUDE = main/mbcs.c main/seccomp.c main/trace.c
MVC_SRCS_CONV = $(sort $(subst /,\\,$(filter-out $(MVC_SRCS_EXCLUDE),$(MVC_SRCS))))

MVC_HEADS = $(MVC_GNULIB_HEADS) $(CMDLINE_HEADS) $(LIB_HEADS) $(OPTLIB2C_HEADS) $(PARSER_HEADS) $(OPTSCRIPT_DSL_HEADS) $(DEBUG_HEADS)  $(WIN32_HEADS)
MVC_HEADS_EXCLUDE = main/interactive_p.h main/mbcs.h main/mbcs_p.h main/trace.h
MVC_HEADS_CONV = $(sort $(subst /,\\,$(filter-out $(MVC_HEADS_EXCLUDE),$(MVC_HEADS))))

MVC_INC_DIRS1 = ..;../main;../gnulib;../parsers;../parsers/cxx;../dsl;
MVC_INC_DIRS2 = ..;../main;../gnulib;../parsers;../parsers/cxx;

# a portable 'echo' which disables the interpretation of escape characters like 'echo -E' on bash
# see https://www.gnu.org/savannah-checkouts/gnu/autoconf/manual/autoconf-2.70/autoconf.html#Limitations-of-Builtins
ECHO = printf '%s\n'
# escape backslashes and newlines in the replacement pattern for sed
ESCAPE_BACKSLASH = sed -e 's|\\|\\\\|g' -e 's/$$/\\/' | sed -e '$$s/\\$$//'
# insert CR before LF except for the last line
LF2CRLF = sed -e '$$!s/$$/\r/'

$(VCXPROJ): $(VCXPROJ).in $(SOURCE_MAK)
	@echo generating $@ ...
	@# C source files \
	SRCS=$$(for i in $(MVC_SRCS_CONV); do \
		$(ECHO) "    <ClCompile Include=\"..\\$$i\" />"; \
	done); \
	SRCS=$$($(ECHO) "$$SRCS" | $(ESCAPE_BACKSLASH)); \
	# header files \
	HEADS=$$(for i in $(MVC_HEADS_CONV); do \
		$(ECHO) "    <ClInclude Include=\"..\\$$i\" />"; \
	done; \
	$(ECHO) "    <ClInclude Include=\"resource.h\" />"); \
	HEADS=$$($(ECHO) "$$HEADS" | $(ESCAPE_BACKSLASH)); \
	# replace @foo@ in $(VCXPROJ).in \
	sed -e "s![@]SRCS[@]!$$SRCS!" \
	    -e "s![@]HEADS[@]!$$HEADS!" \
	    -e "s|[@]INC_DIRS1[@]|${MVC_INC_DIRS1}|" \
	    -e "s|[@]INC_DIRS2[@]|${MVC_INC_DIRS2}|" $< | $(LF2CRLF) > $@

$(VCXPROJ_FILTERS): $(VCXPROJ_FILTERS).in $(SOURCE_MAK)
	@echo generating $@ ...
	@# C source files \
	SRCS=$$(for i in $(MVC_SRCS_CONV); do \
		dirname=$$($(ECHO) $$i | sed -e 's/\\[a-zA-Z_0-9.-]*$$//'); \
		$(ECHO) "    <ClCompile Include=\"..\\$$i\">"; \
		$(ECHO) "      <Filter>Source Files\\$$dirname</Filter>"; \
		$(ECHO) "    </ClCompile>"; \
	done); \
	SRCS=$$($(ECHO) "$$SRCS" | $(ESCAPE_BACKSLASH)); \
	# header files \
	HEADS=$$(for i in $(MVC_HEADS_CONV); do \
		$(ECHO) "    <ClInclude Include=\"..\\$$i\">"; \
		$(ECHO) "      <Filter>Header Files</Filter>"; \
		$(ECHO) "    </ClInclude>"; \
	done; \
	$(ECHO) "    <ClInclude Include=\"resource.h\">"; \
	$(ECHO) "      <Filter>Header Files</Filter>"; \
	$(ECHO) "    </ClInclude>"); \
	HEADS=$$($(ECHO) "$$HEADS" | $(ESCAPE_BACKSLASH)); \
	# replace @foo@ in $(VCXPROJ_FILTERS).in \
	sed -e "s![@]SRCS[@]!$$SRCS!" \
	    -e "s![@]HEADS[@]!$$HEADS!" $< | $(LF2CRLF) > $@
