#	$Id$
#
#	Copyright (c) 1996-2001, Darren Hiebert
#
#	Development makefile for Exuberant Ctags, used to build releases.
#	Requires GNU make.

OBJEXT = o

include source.mak

DSOURCES	=	$(SOURCES) debug.c

DOS_VER_FILES=	ctags.h ctags.1 ctags.lsm NEWS

VERSION_FILES=	$(DOS_VER_FILES) configure.in ctags.spec

LIB_FILES	=	readtags.c readtags.h

COMMON_FILES =	COPYING EXTENDING.html FAQ INSTALL.oth NEWS QUOTES README \
				mk_bc3.mak mk_bc5.mak mk_djg.mak mk_manx.mak mk_ming.mak \
				mk_mpw.mak mk_mvc.mak mk_os2.mak mk_qdos.mak mk_sas.mak \
				source.mak $(DSOURCES) $(HEADERS) $(LIB_FILES)

UNIX_FILES	=	$(COMMON_FILES) \
				INSTALL acconfig.h configure.in \
				Makefile.in maintainer.mak testing.mak \
				descrip.mms mkinstalldirs magic.diff \
				argproc.c mac.c mac.h qdos.c ctags.1 ctags.lsm

DOS_FILES	=	$(COMMON_FILES)

CVS_FILES	=	$(UNIX_FILES)

WARNINGS	=	-Wall -W -Wpointer-arith -Wcast-align -Wwrite-strings \
				-Wmissing-prototypes -Wmissing-declarations \
				-Wnested-externs -Wcast-qual -Wshadow -pedantic \
				-Wstrict-prototypes \
				# -Wtraditional -Wconversion -Werror

ERRFILE	= errors
REDIR	= 2>&1 | tee $(ERRFILE)

RPM_ROOT= $(HOME)/Rpm
CTAGS_DOSDIR = win32
WEB_ARCHIVE_DIR = releases
CTAGS_WEBSITE = website
DEP_DIR	= .deps

CC		= gcc3
INCLUDE	= -I.
DEFS	= -DHAVE_CONFIG_H
COMP_FLAGS = $(INCLUDE) $(DEFS) $(CFLAGS)
OPT		= -O2 -march=i686 -mcpu=i686 
DCFLAGS	= $(COMP_FLAGS) -DDEBUG -DINTERNAL_SORT
LD		= gcc
LDFLAGS	= 

readtags.err: DCFLAGS += -DREADTAGS_MAIN

AUTO_GEN	= configure config.h.in
CONFIG_GEN	= config.cache config.log config.status config.run config.h Makefile
PROF_GEN	= gmon.out
COV_GEN		= *.da *.gcov

#
# Targets
#
ifeq ($(findstring clean,$(MAKECMDGOALS)),)
ifneq ($(MAKECMDGOALS),setup)
ifeq ($(wildcard config.h),)
ctags dctags ctags.prof ctags.cov:
	$(MAKE) config.h
	$(MAKE) $(MAKECMDGOALS)
else
all: dctags tags syntax.vim

-include $(DSOURCES:%.c=$(DEP_DIR)/%.d)

#
# Executable targets
#
ctags: $(SOURCES:.c=.o)
	@ echo "-- Linking $@"
	@ $(LD) -o $@ $(LDFLAGS) $^

dctags: $(SOURCES:.c=.od) debug.od
	@ echo "-- Building $@"
	$(LD) -o $@ $(LDFLAGS) $^ -lefence

mctags: $(SOURCES:.c=.om) debug.om safe_malloc.om
	@ echo "-- Building $@"
	$(LD) -o $@ $(LDFLAGS) $^

ctags.prof: $(SOURCES) $(HEADERS) Makefile
	$(CC) -pg $(COMP_FLAGS) $(WARNINGS) $(SOURCES) -o $@

ctags.cov: $(SOURCES) $(HEADERS) Makefile
	$(CC) -fprofile-arcs -ftest-coverage $(COMP_FLAGS) $(WARNINGS) $(SOURCES) -o $@

gcov: $(SOURCES:.c=.c.gcov)

readtags: readtags.[ch]
	$(CC) -g $(COMP_FLAGS) -DREADTAGS_MAIN -o $@ readtags.c

readtags.o: readtags.[ch]
	$(CC) $(COMP_FLAGS) -c readtags.c

etyperef: routines.c etyperef.o keyword.o strlist.o vstring.o
	$(CC) $(OPT) $(COMP_FLAGS) -o $@ $^

etyperef.o: eiffel.c
	$(CC) -DTYPE_REFERENCE_TOOL $(OPT) $(COMP_FLAGS) -o $@ -c $<

endif
endif
endif

ctags32.exe: $(SOURCES) $(HEADERS)
	gcc-dos -DMSDOS -O2 -Wall -s -o $@ $(SOURCES)

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
	rm -f *.[ois] *.o[dm] ctags dctags mctags ctags*.exe readtags \
		ctags.html ctags.prof ctags.cov *.bb *.bbg tags TAGS syntax.vim \
		$(ERRFILE)

distclean: clean
	rm -f $(CONFIG_GEN)

maintainer-clean maintclean: distclean
	rm -f $(AUTO_GEN)

ctags.man: ctags.1
	groff -Tascii -mandoc $< | sed 's/.//g' > $@

ctags.html: ctags.1
	man2html $< > $@

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
include testing.mak

#
# CVS management
#
status:
	@ cvs -n -q update

cvs-retag-%: CVS_TAG_OPTIONS := -F

cvs-tag-% cvs-retag-%: cvs-tagcheck-%
	@ echo "---------- Tagging release `echo $* | sed 's/\./_/g'`"
	@ cvs tag -c $(CVS_TAG_OPTIONS) Ctags-`echo $* | sed 's/\./_/g'`

cvs-tagcheck-%:
	@ if test -z "$(CVS_TAG_OPTIONS)"; then \
		if cvs update -p -r Ctags-`echo $* | sed 's/\./_/g'` maintainer.mak >/dev/null 2>&1 ;then \
			echo "release-$* already exists; use rerelease-$*" >&2 ;\
			exit 1 ;\
		fi ;\
	fi

cvs-files:
	@ls -1 $(CVS_FILES)

#
# Web site files
#
website-%: website-man-% website-index-% $(CTAGS_WEBSITE)/news.html \
		$(CTAGS_WEBSITE)/EXTENDING.html
	:

website-man-%: ctags.1 Makefile
	@ echo "---------- Generating $(CTAGS_WEBSITE)/ctags.html"
	umask 022 ; \
	man2html $< | sed -e "s/@@VERSION@@/$*/g" \
		-e 's%<A HREF="mailto:[^"]*">\([^@]*\)@\([^<]*\)</A>%\1\ at \2%' \
		> $(CTAGS_WEBSITE)/ctags.html

website-index-%: index.html Makefile
	@ echo "---------- Generating $(CTAGS_WEBSITE)/index.html"
	umask 022 ; \
	sed -e "s/@@VERSION@@/$*/g" \
		-e "s/@@DOS_VERSION@@/`echo $* | sed 's/\.//g'`/g" \
		-e "s/@@DATE@@/`date +'%d %B %Y'`/" \
		$< > $(CTAGS_WEBSITE)/index.html

$(CTAGS_WEBSITE)/EXTENDING.html: EXTENDING.html
	@ echo "---------- Generating $(CTAGS_WEBSITE)/EXTENDING.html"
	cp $< $@ && chmod 644 $@

$(CTAGS_WEBSITE)/news.html: NEWS Makefile
	@ echo "---------- Generating $(CTAGS_WEBSITE)/news.html"
	umask 022 ; \
	sed -e 's/</\&lt;/g' -e 's/>/\&gt;/g' \
		-e 's@^Current Version:.*$$@<html><head><title>Exuberant Ctags: Change Notes</title></head><body><h1>Change Notes</h1><pre>@' \
		-e 's@\(^ctags-.* (.*)\)$$@<b>\1</b>@' \
		-e 's@^vim:.*$$@</pre><hr><a href="http:index.html">Back to <strong>Exuberant Ctags</strong></a></body></html>@' \
		$< > $@

#
# Release management
#
ctags-%.lsm: ctags.lsm
	sed -e "s/@@VERSION@@/$*/" -e "s/@@LSMDATE@@/`date +'%Y-%m-%d'`/" $< > $@

ctags-%.tar.gz: $(UNIX_FILES) $(VERSION_FILES)
	@ echo "---------- Building tar ball"
	if [ -d ctags-$* ] ;then rm -fr ctags-$** ;fi
	mkdir ctags-$*
	cp -p $(UNIX_FILES) ctags-$*/
	for file in $(VERSION_FILES) ;do \
		rm -f ctags-$*/$${file} ;\
		sed -e "s/@@VERSION@@/$*/" \
		    -e "s/@@LSMDATE@@/`date +'%Y-%m-%d'`/" \
			$${file} > ctags-$*/$${file} ;\
	done
	chmod 644 ctags-$*/*
	chmod 755 ctags-$*/mkinstalldirs
	(cd ctags-$*; autoheader; chmod 644 config.h.in)
	(cd ctags-$*; autoconf; chmod 755 configure)
	(cd ctags-$*; man2html ctags.1 > ctags.html)
	tar -zcf $@ ctags-$*

ctags-%.tar.Z: ctags-%.tar.gz
	tar -Zcf $@ ctags-$*

$(CTAGS_DOSDIR)/ctags%: FORCE
	if [ -d $(CTAGS_DOSDIR)/ctags$* ] ;\
		then rm -fr $(CTAGS_DOSDIR)/ctags$*/* ;\
		else mkdir -p $(CTAGS_DOSDIR)/ctags$* ;\
	fi

dos1-%: $(DOS_FILES)
	for file in $^ ;do \
		unix2dos < $${file} > $(CTAGS_DOSDIR)/ctags$*/$${file} ;\
	done
	cd $(CTAGS_DOSDIR); sed -e "s/@@DOS_VERSION@@/$*/" makefile.in > makefile

dos2-%: $(DOS_VER_FILES)
	for file in $^ ;do \
		rm -f $(CTAGS_DOSDIR)/ctags`echo $*|sed 's/\.//g'`/$${file} ;\
		sed -e "s/@@VERSION@@/$*/" \
		    -e "s/@@LSMDATE@@/`date +'%d%b%y' | tr 'a-z' 'A-Z'`/" $${file} |\
			unix2dos > $(CTAGS_DOSDIR)/ctags`echo $*|sed 's/\.//g'`/$${file} ;\
	done ;\
	cd $(CTAGS_DOSDIR)/ctags`echo $*|sed 's/\.//g'` ; man2html ctags.1 > ctags.html

dos-%:
	@ echo "---------- Building MSDOS release directory"
	$(MAKE) $(CTAGS_DOSDIR)/ctags`echo $*|sed 's/\.//g'` \
			dos1-`echo $*|sed 's/\.//g'` dos2-$*

$(RPM_ROOT)/SOURCES $(RPM_ROOT)/SPECS:
	mkdir -p $@

rpm-%: ctags-%.tar.gz ctags.spec $(RPM_ROOT)/SOURCES $(RPM_ROOT)/SPECS 
	@ echo "---------- Building RPM"
	cp -p ctags-$*.tar.gz $(RPM_ROOT)/SOURCES/
	sed -e "s/@@VERSION@@/$*/" ctags.spec > $(RPM_ROOT)/SPECS/ctags-$*.spec
	(cd $(RPM_ROOT)/SPECS; CC=gcc3 rpm -ba ctags-$*.spec)
	rm -fr $(RPM_ROOT)/BUILD/ctags-$*

ctags32-%: ctags-%.tar.gz
	@ echo "---------- Building DPMS binary for MSDOS"
	(cd ctags-$*; $(MAKE) -f ../Makefile ctags32.exe; mv ctags32.exe ..)
	rm -f $(CTAGS_DOSDIR)/ctags32.exe
	mcopy ctags32.exe $(CTAGS_DOSDIR)

#
# Prevent make from deleting these automatically
#
.PRECIOUS: ctags-%.tar.gz ctags-%.tar.Z

cleanrelease-%:
	rm -f ctags-$*.tar.gz
	rm -fr ctags-$*
	rm -fr $(CTAGS_DOSDIR)/ctags`echo $*|sed 's/\.//g'`
	rm -f $(RPM_ROOT)/SOURCES/ctags-$*.tar.gz
	rm -f $(RPM_ROOT)/RPMS/i386/ctags-$*-1.i386.rpm
	rm -f $(RPM_ROOT)/SRPMS/ctags-$*-1.src.rpm
	rm -f $(RPM_ROOT)/SPECS/ctags-$*.spec

internal-release-%: ctags-%.tar.gz ctags-%.tar.Z dos-% rpm-% website-%
	@ echo "---------- Copying files to web archive"
	cp -p ctags-$*.tar.* $(WEB_ARCHIVE_DIR)
	cp -p $(RPM_ROOT)/RPMS/i386/ctags-$*-1.i386.rpm $(WEB_ARCHIVE_DIR)
	cp -p $(RPM_ROOT)/SRPMS/ctags-$*-1.src.rpm $(WEB_ARCHIVE_DIR)
	cp -p ctags-$*/ctags.lsm $(WEB_ARCHIVE_DIR)/ctags-$*.lsm
	chmod o+r $(WEB_ARCHIVE_DIR)/*
	@ echo "---------- Release $* completed"

release-%: cvs-tag-% internal-release-%
	:

rerelease-%: cvs-retag-% internal-release-%
	:

#
# Dependency file generation
#
$(DEP_DIR)/%.d: %.c maintainer.mak
	@ if [ ! -d $(DEP_DIR) ] ;then mkdir -p $(DEP_DIR) ;fi
	@ $(CC) -M $(DCFLAGS) $< | sed 's/\($*\.o\)\([ :]\)/\1 $*.od $*.om $(@F)\2/g' > $@


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

%.om: %.c
	@ echo "-- Compiling (safe alloc) $<"
	@ $(CC) -g -DTRAP_MEMORY_CALLS $(DCFLAGS) $(WARNINGS) -o $*.om -c $<

%.i: %.c FORCE
	$(CC) $(DCFLAGS) $(WARNINGS) -Wuninitialized -O -E $< > $@

%.ic: %.c FORCE
	$(CC) $(DCFLAGS) $(WARNINGS) -Wuninitialized -O -E $< | noblanks > $@

%.s: %.c FORCE
	$(CC) $(DCFLAGS) $(WARNINGS) -S $< > $@

%.err: %.c
	@ $(CC) $(DCFLAGS) $(WARNINGS) -Wuninitialized -O -c $<
	@ rm $*.o

%.c.gcov: %.da
	@ gcov $*.c

%.sproto: %.c
	@ genproto -s -m __ARGS $<

%.proto: %.c
	@ genproto -e -m __ARGS $<

# vi:ts=4 sw=4
