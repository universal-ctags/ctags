#	$Id$
#
#	Copyright (c) 1996-2007, Darren Hiebert
#
#	Development makefile for Exuberant Ctags. Also used to build releases.
#	Requires GNU make.

OBJEXT := o

include source.mak

DSOURCES     := $(SOURCES) debug.c

VERSION_FILES:= ctags.h ctags.1 NEWS

LIB_FILES    := readtags.c readtags.h

ENVIRONMENT_MAKEFILES := \
				mk_bc3.mak mk_bc5.mak mk_djg.mak mk_manx.mak mk_ming.mak \
				mk_mpw.mak mk_mvc.mak mk_os2.mak mk_qdos.mak mk_sas.mak \

COMMON_FILES := COPYING EXTENDING.html FAQ INSTALL.oth MAINTAINERS NEWS README \
				$(ENVIRONMENT_MAKEFILES) source.mak \
				$(DSOURCES) $(HEADERS) $(LIB_FILES) \
				$(ENVIRONMENT_SOURCES) $(ENVIRONMENT_HEADERS)

UNIX_FILES   := $(COMMON_FILES) \
				.indent.pro INSTALL acconfig.h configure.in \
				Makefile.in maintainer.mak testing.mak \
				descrip.mms mkinstalldirs magic.diff \
				ctags.spec ctags.1

WIN_FILES    := $(COMMON_FILES) $(VERSION_FILES)
WIN_REGEX    := regex.c regex.h
 
SVN_FILES    := $(UNIX_FILES)

WARNINGS     := -Wall -W -Wpointer-arith -Wcast-align -Wwrite-strings \
				-Wmissing-prototypes -Wmissing-declarations \
				-Wnested-externs -Wcast-qual -Wshadow -pedantic \
				-Wstrict-prototypes \
				# -Wtraditional -Wconversion -Werror

PRODUCER := Darren B. Hiebert
EMAIL := dhiebert@users.sourceforge.net
CTAGS_WEBSITE := http://ctags.sourceforge.net
RPM_ROOT := rpms
RPM_ABS_ROOT := $(PWD)/$(RPM_ROOT)
WINDOWS_DIR := win32
RELEASE_DIR := releases
CTAGS_WEBDIR := website
win_version = $(subst .,,$(version))
DEP_DIR := .deps
HOST_ARCH := $(shell uname -p)

ifneq ($(findstring $(HOST_ARCH),i386 i686),)
COMP_ARCH := -march=i686
endif

CC         := gcc
INCLUDE    := -I.
DEFS       := -DHAVE_CONFIG_H
COMP_FLAGS := $(INCLUDE) $(DEFS) $(CFLAGS)
PROF_OPT   := -O3 $(COMP_ARCH)
#OPT        := $(PROF_OPT) -fomit-frame-pointer
OPT        := $(PROF_OPT)
DCFLAGS    := $(COMP_FLAGS) -DDEBUG -DINTERNAL_SORT
LD         := gcc
LDFLAGS    := 
RPM_FLAGS  := -O3 $(COMP_ARCH)

AUTO_GEN   := configure config.h.in
CONFIG_GEN := config.cache config.log config.status config.run config.h Makefile
PROF_GEN   := gmon.out
COV_GEN	   := *.da *.gcov

UNIX2DOS := perl -pe 's/$$/\r/'
MAN2HTML := tbl | groff -Wall -mtty-char -mandoc -Thtml -c

#
# Targets
#
ifeq ($(findstring clean,$(MAKECMDGOALS)),)
ifeq ($(wildcard config.h),)
ctags dctags ctags.prof ctags.cov:
	$(MAKE) config.h
	$(MAKE) $(MAKECMDGOALS)
else
all: dctags tags syntax.vim

-include $(DSOURCES:%.c=$(DEP_DIR)/%.d) $(DEP_DIR)/readtags.d

#
# Executable targets
#
ctags: $(SOURCES:.c=.o)
	@ echo "-- Linking $@"
	@ $(LD) -o $@ $(LDFLAGS) $^

dctags: $(SOURCES:.c=.od) debug.od
	@ echo "-- Building $@"
	$(LD) -o $@ $(LDFLAGS) $^

ctags.prof: $(SOURCES) $(HEADERS) Makefile
	$(CC) -pg $(PROF_OPT) $(COMP_FLAGS) $(WARNINGS) $(SOURCES) -o $@

ctags.cov: $(SOURCES) $(HEADERS) Makefile
	$(CC) -fprofile-arcs -ftest-coverage $(COMP_FLAGS) $(WARNINGS) $(SOURCES) -o $@

gcov: $(SOURCES:.c=.c.gcov)

readtags: readtags.[ch]
	$(CC) -g $(COMP_FLAGS) -DDEBUG -DREADTAGS_MAIN -o $@ readtags.c

readtags.o: readtags.c readtags.h
	$(CC) $(COMP_FLAGS) -c readtags.c

etyperef: etyperef.o keyword.o routines.o strlist.o vstring.o
	$(CC) -o $@ $^

etyperef.o: eiffel.c
	$(CC) -DTYPE_REFERENCE_TOOL $(OPT) $(COMP_FLAGS) -o $@ -c $<

endif
endif

#
# Support targets
#
FORCE:

config.h.in: acconfig.h configure.in
	autoheader
	@ touch $@

configure: configure.in
	autoconf

config.status: configure
	./config.status --recheck

config.h: config.h.in config.status
	./config.status
	touch $@

depclean:
	rm -f $(DEP_DIR)/*.d

profclean:
	rm -f $(PROF_GEN)

gcovclean:
	rm -f $(COV_GEN)

clean: depclean profclean gcovclean clean-test
	rm -f *.[ois] *.o[dm] ctags dctags ctags*.exe readtags etyperef \
		ctags.man ctags.html ctags.prof ctags.cov *.bb *.bbg tags TAGS syntax.vim

distclean: clean
	rm -f $(CONFIG_GEN)

maintainer-clean maintclean: distclean
	rm -f $(AUTO_GEN)

%.man: %.1 Makefile
	tbl $< | groff -Wall -mtty-char -mandoc -Tascii -c | sed 's/.//g' > $@

%.html: %.1 Makefile
	cat $< | $(MAN2HTML) > $@

tags: $(DSOURCES) $(HEADERS) $(LIB_FILES) Makefile *.mak
	@ echo "-- Building tag file"
	@ ctags *

#
# Create a Vim syntax file for all typedefs
#
syntax: syntax.vim
syntax.vim: $(DSOURCES) $(HEADERS) $(LIB_FILES)
	@ echo "-- Generating syntax file"
	@ ctags --c-types=cgstu --file-scope -o- $^ |\
		awk '{print $$1}' | sort -u | fmt |\
		awk '{printf("syntax keyword Typedef\t%s\n", $$0)}' > $@

#
# Testing
#
-include testing.mak

#
# Release management
#

.SECONDARY:

RPM_ARCH := i386
RPM_SUBDIRS := BUILD SOURCES SPECS SRPMS RPMS
RPM_DIRS := $(addprefix $(RPM_ROOT)/,$(RPM_SUBDIRS))

$(RELEASE_DIR)/ctags-%-1.$(RPM_ARCH).rpm: \
		$(RPM_ROOT)/RPMS/$(RPM_ARCH)/ctags-%-1.$(RPM_ARCH).rpm \
		| $(RELEASE_DIR)
	ln -f $< $@
	chmod 644 $@

$(RELEASE_DIR)/ctags-%-1.src.rpm: \
		$(RPM_ROOT)/SRPMS/ctags-%-1.src.rpm \
		| $(RELEASE_DIR)
	ln -f $< $@
	chmod 644 $@

$(eval $(RPM_DIRS) $(RELEASE_DIR): ; mkdir -p $$@)

$(RPM_ROOT)/SRPMS/ctags-%-1.src.rpm \
$(RPM_ROOT)/RPMS/$(RPM_ARCH)/ctags-%-1.$(RPM_ARCH).rpm: \
		$(RPM_ROOT)/SOURCES/ctags-%.tar.gz \
		$(RPM_ROOT)/SPECS/ctags-%.spec \
		| $(RPM_DIRS)
	rpmbuild --define '_topdir $(RPM_ABS_ROOT)' --define 'optflags $(RPM_FLAGS)' --define 'packager $(PRODUCER) $(CTAGS_WEBSITE)' -ba $(RPM_ROOT)/SPECS/ctags-$*.spec
	rm -fr $(RPM_ROOT)/BUILD/ctags-$*

$(RPM_ROOT)/rpmrc: rpmmacros maintainer.mak
	echo "optflags: $(RPM_ARCH) $(RPM_FLAGS)" > $@
	echo "macrofiles: $(PWD)/rpmmacros" >> $@

$(RPM_ROOT)/rpmmacros: maintainer.mak
	echo "%_topdir $(RPM_ABS_ROOT)" > $@
	echo '%_gpg_name "$(PRODUCER) <$(EMAIL)>"' >> $@
	echo "%packager $(PRODUCER) $(CTAGS_WEBSITE)" >> $@
	echo "%_i18ndomains %{nil}" >> $@
	echo "%debug_package %{nil}" >> $@

$(RPM_ROOT)/SPECS/ctags-%.spec: ctags.spec | $(RPM_ROOT)/SPECS
	sed -e "s/@VERSION@/$*/" ctags.spec > $(RPM_ROOT)/SPECS/ctags-$*.spec

$(RPM_ROOT)/SOURCES/ctags-%.tar.gz: $(RELEASE_DIR)/ctags-%.tar.gz | $(RPM_ROOT)/SOURCES
	ln -f $< $@

$(RELEASE_DIR)/ctags-%.tar.gz: $(UNIX_FILES) | $(RELEASE_DIR)
	@ echo "---------- Building tar ball"
	if [ -d $(@D)/dirs/ctags-$* ]; then rm -fr $(@D)/dirs/ctags-$*; fi
	mkdir -p $(@D)/dirs/ctags-$*
	cp -p $(UNIX_FILES) $(@D)/dirs/ctags-$*/
	sed -e 's/\(PROGRAM_VERSION\) "\([^ ]*\)"/\1 "$*"/' ctags.h > $(@D)/dirs/ctags-$*/ctags.h
	sed -e 's/"\(Version\) \([^ ]*\)"/"\1 $*"/' ctags.1 > $(@D)/dirs/ctags-$*/ctags.1
	sed -e 's/\(Current Version:\) [^ ]*/\1 $*/' -e 's/@VERSION@/$*/' -e "s/@DATE@/`date +'%d %b %Y'`/" NEWS > $(@D)/dirs/ctags-$*/NEWS
	(cd $(@D)/dirs/ctags-$* ;\
		chmod 644 * ;\
		chmod 755 mkinstalldirs ;\
		autoheader ;\
		chmod 644 config.h.in ;\
		autoconf ;\
		chmod 755 configure ;\
		rm -fr autom4te.cache ;\
		cat ctags.1 | $(MAN2HTML) > ctags.html ;\
	)
	cd $(@D)/dirs && tar -zcf ../$(@F) ctags-$*
	chmod 644 $@

clean-rpm:
	rm -fr $(RPM_ROOT)

ifneq ($(findstring win-,$(MAKECMDGOALS)),)
ifeq ($(version),,)
$(error $(MAKECMDGOALS) target requires value for 'version')
endif
endif

check-version-%:
	@ if [ -z "$(version)" ]; then echo "target requires value for 'version'" >&2; exit 1; fi

$(WINDOWS_DIR)/ctags$(win_version): \
		$(RELEASE_DIR)/ctags-$(version).tar.gz maintainer.mak \
		| $(WINDOWS_DIR)
	@ echo "---------- Building Win32 release directory"
	rm -fr "$(WINDOWS_DIR)/ctags$(win_version)"
	mkdir -p "$(WINDOWS_DIR)/ctags$(win_version)"
	for file in $(WIN_FILES) ctags.html; do \
		$(UNIX2DOS) < "$(RELEASE_DIR)/dirs/ctags-$(version)/$${file}" > $@/$${file} ;\
	done
	for file in $(WIN_REGEX); do \
		$(UNIX2DOS) < "gnu_regex/$${file}" > $@/$${file} ;\
	done

$(RELEASE_DIR)/ctags%.zip: \
		check-version-% \
		$(WINDOWS_DIR)/ctags% \
		$(WINDOWS_DIR)/ctags%/ctags.exe
	cd $(WINDOWS_DIR) && zip -r ../$@ ctags$*

win-source: $(WINDOWS_DIR)/ctags$(win_version)

win-bin:
	$(RELEASE_DIR)/ctags$(win_version).zip

release-win-%:
	$(MAKE) version="$*" win-source

release-tar-%: $(RELEASE_DIR)/ctags-%.tar.gz
	:

release-rpm-%: \
		$(RELEASE_DIR)/ctags-%-1.$(RPM_ARCH).rpm \
		$(RELEASE_DIR)/ctags-%-1.src.rpm
	:

release-source-%: $(RELEASE_DIR)/ctags-%.tar.gz
	$(MAKE) version="$*" win-source

release-bin-%: \
		$(RELEASE_DIR)/ctags-%-1.$(RPM_ARCH).rpm \
		$(RELEASE_DIR)/ctags-%-1.src.rpm
	$(MAKE) version="$*" win-bin

$(WINDOWS_DIR):
	mkdir -p $@

#
# Web site files
#
website-%: website-man-% website-index-% website-news-% \
		$(CTAGS_WEBDIR)/EXTENDING.html
	:

website-man-%: ctags.1 Makefile
	@ echo "---------- Generating $(CTAGS_WEBDIR)/ctags.html"
	umask 022 ; \
	sed -e 's/"\(Version\) \([^ ]*\)"/"\1 $*"/' ctags.1 |\
	$(MAN2HTML) > $(CTAGS_WEBDIR)/ctags.html

website-index-%: index.html Makefile
	@ echo "---------- Generating $(CTAGS_WEBDIR)/index.html"
	umask 022 ; \
	sed -e "s/@VERSION@/$*/g" \
		-e "s/@DOS_VERSION@/`echo $* | sed 's/\.//g'`/g" \
		-e "s/@DATE@/`date +'%d %B %Y'`/" \
		$< > $(CTAGS_WEBDIR)/index.html

website-news-%: NEWS maintainer.mak
	@ echo "---------- Generating $(CTAGS_WEBDIR)/news.html"
	umask 022 ; \
	sed -e 's/\(Current Version:\) [^ ]*/\1 $*/' \
	    -e 's/@VERSION@/$*/' \
		-e "s/@DATE@/`date +'%d %b %Y'`/" \
		-e 's/</\&lt;/g' -e 's/>/\&gt;/g' \
		-e 's@^Current Version:.*$$@<html><head><title>Exuberant Ctags: Change Notes</title></head><body><h1>Change Notes</h1><pre>@' \
		-e 's@\(^ctags-.* (.*)\)$$@<b>\1</b>@' \
		-e 's@^vim:.*$$@</pre><hr><a href="http:index.html">Back to <strong>Exuberant Ctags</strong></a></body></html>@' \
		$< > $(CTAGS_WEBDIR)/news.html

$(CTAGS_WEBDIR)/EXTENDING.html: EXTENDING.html
	@ echo "---------- Generating $(CTAGS_WEBDIR)/EXTENDING.html"
	cp $< $@ && chmod 644 $@

#
# SVN management
#
svn_url := https://ctags.svn.sourceforge.net/svnroot/ctags

release-svn-%: svn-tagcheck-%
	@ echo "---------- Tagging release $*"
	@ echo svn copy -m'Release of ctags-$*' $(svn_url)/trunk $(svn_url)/tags/ctags-$*

rerelease-svn-%:
	@ echo "---------- Tagging release $*"
	@ echo svn remove -m'Regenerating release of ctags-$*' $(svn_url)/tags/ctags-$*
	@ echo svn copy -m'Release of ctags-$*' $(svn_url)/trunk $(svn_url)/tags/ctags-$*

svn-tagcheck-%:
	if svn list $(svn_url)/tags/ | grep -q 'ctags-$*/$$' >/dev/null 2>&1 ;then \
		echo "ctags-$* already exists; use rerelease-$*" >&2 ;\
		exit 1 ;\
	fi

svn-files:
	@ls -1 $(SVN_FILES)

#
# Dependency file generation
#
$(DEP_DIR)/%.d: %.c maintainer.mak
	@ if [ ! -d $(DEP_DIR) ] ;then mkdir -p $(DEP_DIR) ;fi
	@ $(CC) -M $(DCFLAGS) $< | sed 's/\($*\.o\)\([ :]\)/\1 $*.od $(@F)\2/g' > $@


%.inc: %.c Makefile
	-@ $(CC) -MM $(DCFLAGS) $<

#
# Compilation rules
#
%.o: %.c
	@ echo "-- Compiling $<"
	@ $(CC) $(COMP_FLAGS) -DEXTERNAL_SORT $(OPT) $(WARNINGS) -Wuninitialized -c $<

%.od: %.c
	@ echo "-- Compiling (debug) $<"
	@ $(CC) -g $(DCFLAGS) $(WARNINGS) -o $*.od -c $<

%.i: %.c FORCE
	$(CC) $(DCFLAGS) $(WARNINGS) -Wuninitialized -O -E $< > $@

%.ic: %.c FORCE
	$(CC) $(DCFLAGS) $(WARNINGS) -Wuninitialized -O -E $< | sed '/^[	]*$/d' > $@

%.s: %.c FORCE
	$(CC) $(DCFLAGS) $(WARNINGS) -S $< > $@

readtags.err: DCFLAGS += -DREADTAGS_MAIN

%.err: %.c
	@ $(CC) $(DCFLAGS) $(WARNINGS) -Wuninitialized -O -c $<
	@ rm $*.o

%.c.gcov: %.da
	@ gcov $*.c

%.sproto: %.c
	@ genproto -s -m __ARGS $<

%.proto: %.c
	@ genproto -e -m __ARGS $<

# Print out the value of a variable
# From http://www.cmcrossroads.com/ubbthreads/showflat.php?Cat=0&Board=cmbasics&Number=28829
print-%:
	@echo $* = $($*)

# Print out the expanded values of all variables
# From http://www.cmcrossroads.com/ubbthreads/showflat.php?Cat=0&Number=29581
.PHONY: print-vars
print-vars:
	@$(foreach V,$(sort $(.VARIABLES)), \
		$(if $(filter-out environment% default automatic, \
			$(origin $V)),$(warning $V=$($V))))

# Print out the declared values of all variables
.PHONY: print-vars-decl
print-vars-decl:
	@$(foreach V,$(sort $(.VARIABLES)), \
		$(if $(filter-out environment% default automatic, \
			$(origin $V)),$(warning $V=$(value $V))))

# vi:ts=4 sw=4
