#
# This is a Makefile.am read by the Autotools.
#
#	Copyright (c) 2017, Masatake YAMMATO
#	Copyright (c) 2017, Red Hat, Inc.
#
#	This source code is released for free distribution under the terms
#	of the GNU General Public License version 2 or (at your option) any
#	later version.
#

#SUFFIXES = .rst.in .rst .html .pdf

VERBOSE   = $(VERBOSE_@AM_V@)
VERBOSE_  = $(VERBOSE_@AM_DEFAULT_V@)
VERBOSE_0 = @

BUILD_OPTIONS   = $(BUILD_OPTIONS_@AM_V@)
BUILD_OPTIONS_  = $(BUILD_OPTIONS_@AM_DEFAULT_V@)
BUILD_OPTIONS_1 = -v

#############################################################################

# https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/Preset-Output-Variables.html
# in this document, we use `abs_srcdir`, this is an absolute dir of this file. to prevent: No such file or directory

rst:
	$(VERBOSE)$(abs_srcdir)/build.sh $(BUILD_OPTIONS) gen-rst

man:
	$(VERBOSE)$(abs_srcdir)/build.sh $(BUILD_OPTIONS) rst2man  $(RST2MAN_OPTIONS)

pdf:
	$(VERBOSE)$(abs_srcdir)/build.sh $(BUILD_OPTIONS) rst2pdf  $(RST2PDF_OPTIONS)

html:
	$(VERBOSE)$(abs_srcdir)/build.sh $(BUILD_OPTIONS) rst2html $(RST2HTML_OPTIONS)

# remove generated files to be commited (used for `make checkgen`)
clean-docs:
	$(VERBOSE)$(abs_srcdir)/build.sh $(BUILD_OPTIONS) clean rst doc

update-docs:
	$(VERBOSE)$(abs_srcdir)/build.sh $(BUILD_OPTIONS) update-docs

.PHONY: rst man pdf html clean-docs update-docs

#############################################################################

# this target will be automatically invoked when you run `make clean`
# https://www.gnu.org/software/automake/manual/html_node/Extending.html
clean-local:
	$(VERBOSE)$(abs_srcdir)/build.sh $(BUILD_OPTIONS) clean rst man pdf html doc

# this target will be automatically invoked when you run `make maintainer-clean`
# https://www.gnu.org/software/automake/manual/html_node/Extending.html
maintainer-clean-local:
	$(VERBOSE)$(abs_srcdir)/build.sh $(BUILD_OPTIONS) clean rst

# this target will be automatically invoked when you run `make dist`
# https://www.gnu.org/software/automake/manual/html_node/The-dist-Hook.html
dist-hook: rst
	$(VERBOSE)for item in $$(cd $(abs_srcdir) && ls *.rst*) build.sh ; do cp -f $(BUILD_OPTIONS) $(abs_srcdir)/$$item $(distdir)/$$item; done

# this target will be automatically invoked when you run `make install`
# https://www.gnu.org/software/automake/manual/html_node/Extending.html
install-exec-hook:
	$(VERBOSE)if $(abs_srcdir)/build.sh $(BUILD_OPTIONS) find-exec rst2man > /dev/null ; then \
				 $(abs_srcdir)/build.sh $(BUILD_OPTIONS) rst2man $(RST2MAN_OPTIONS) ; \
		         $(abs_srcdir)/build.sh $(BUILD_OPTIONS) install-mans $(mandir) ; else \
	printf '%s\n' "NOTE: DO NOT INSTALL MAN-PAGES due to rst2man not found."; fi
