#
# This is a Makefile.am read by the Autotools.
# GNU Make is needed.
#
#	Copyright (c) 2017, Masatake YAMMATO
#	Copyright (c) 2017, Red Hat, Inc.
#
#	This source code is released for free distribution under the terms
#	of the GNU General Public License version 2 or (at your option) any
#	later version.
#

# Note: RST2MAN, RST2HTML, RST2PDF, and RST2MAN_OPTIONS are defined by configure.ac
RST2MAN_FLAGS = $(RST2MAN_OPTIONS)
RST2HTML_FLAGS =
RST2PDF_FLAGS =
DOCS_DIR = ../docs/man

SUFFIXES = .rst .html .pdf .in

# The man_MANS or dist_man_MANS requires conventional file names for man files.
# For example `$(basename $(files))` is not accepted.
GEN_IN_MAN_FILES = \
	ctags.1 \
	ctags-optlib.7 \
	ctags-incompatibilities.7 \
	ctags-client-tools.7 \
	ctags-faq.7 \
	\
	ctags-lang-gdscript.7 \
	ctags-lang-iPythonCell.7 \
	ctags-lang-julia.7 \
	ctags-lang-python.7 \
	ctags-lang-tcl.7 \
	ctags-lang-verilog.7 \
	ctags-lang-inko.7 \
	ctags-lang-r.7 \
	ctags-lang-rmarkdown.7 \
	ctags-lang-sql.7 \
	\
	readtags.1 \
	tags.5 \
	\
	$(NULL)

man_pages  = $(GEN_IN_MAN_FILES)
man_in_files = $(addsuffix .in,$(man_pages))
rst_files  = $(addsuffix .rst,$(man_pages))
html_pages = $(addsuffix .html,$(man_pages))
pdf_pages  = $(addsuffix .pdf,$(man_pages))
doc_files  = $(addprefix $(DOCS_DIR)/,$(rst_files))

# for automake
# generate man pages only when rst2man is installed
if HAVE_RST2MAN
man_MANS = $(man_pages)
endif
EXTRA_DIST = README $(addsuffix .rst.in,$(GEN_IN_MAN_FILES)) $(man_in_files)
CLEANFILES = $(man_pages) $(addsuffix .rst,$(GEN_IN_MAN_FILES)) $(html_pages) $(pdf_pages)
MAINTAINERCLEANFILES =  $(man_in_files)

.PHONY: man man-in html pdf update-docs clean-docs

all-am: update-docs

man: $(man_pages)
man-in: $(man_in_files)
html: $(html_pages)
pdf: $(pdf_pages)

# use `[@]` not to be replaced by automake
REPLACE_CONF_VARS =	sed \
		-e s/[@]CTAGS_NAME_EXECUTABLE[@]/ctags/g \
		-e s/[@]ETAGS_NAME_EXECUTABLE[@]/etags/g \
		-e s/[@]VERSION[@]/$(VERSION)/g

rst2man_verbose    = $(rst2man_verbose_@AM_V@)
rst2man_verbose_   = $(rst2man_verbose_@AM_DEFAULT_V@)
rst2man_verbose_0  = @echo RST2MAN "   $@";

rst2html_verbose   = $(rst2html_verbose_@AM_V@)
rst2html_verbose_  = $(rst2html_verbose_@AM_DEFAULT_V@)
rst2html_verbose_0 = @echo RST2HTML "  $@";

rst2pdf_verbose    = $(rst2pdf_verbose_@AM_V@)
rst2pdf_verbose_   = $(rst2pdf_verbose_@AM_DEFAULT_V@)
rst2pdf_verbose_0  = @echo RST2MAN "   $@";

sed_verbose        = $(sed_verbose_@AM_V@)
sed_verbose_       = $(sed_verbose_@AM_DEFAULT_V@)
sed_verbose_0      = @echo SED "       $@";

rm_verbose         = $(rm_verbose_@AM_V@)
rm_verbose_        = $(rm_verbose_@AM_DEFAULT_V@)
rm_verbose_0       = @echo RM "        $@";

#
# generate man
#
# generate *.in to be distributed
%.in: %.rst.in
if HAVE_RST2MAN
	$(rst2man_verbose)$(RST2MAN) $(RST2MAN_FLAGS) $< > $@
else
	$(error Cannot make $@: rst2man is not installed)
endif

%: %.in
	$(sed_verbose)$(REPLACE_CONF_VARS) < $< > $@

#
# generate HTML and PDF
#
%.rst: %.rst.in
	$(sed_verbose)$(REPLACE_CONF_VARS) < $< > $@

%.html: %.rst
if HAVE_RST2HTML
	$(rst2html_verbose)$(RST2HTML) $(RST2HTML_FLAGS) $< > $@
else
	$(error Cannot make $@: rst2html is not installed)
endif

%.pdf: %.rst
if HAVE_RST2PDF
	$(rst2pdf_verbose)$(RST2PDF) $(RST2PDF_FLAGS) $< -o $@
else
	$(error Cannot make $@: rst2pdf is not installed)
endif

#
# generate $(DOCS_DIR)/*.rst for Sphinx document
#
update-docs: $(doc_files)

# Convert the names of man page files (without suffix) to man page citations.
#   ctags.1 tags.5 ... => ctags(1) tags(5) ...
cites := ${subst .,(,${addsuffix ),${man_pages}}}

# Convert the man page citations to sed patterns for making hyperlinks.
#   ctags(1) tags(5) ...
#     => -e 's/\<ctags(1)/:ref:`& <&>`/g' -e 's/\<ctags(5)/:ref:`& <&>`/g' ...
if HAS_GNU_SED
reppat := $(foreach m,$(cites),-e 's/\<$(m)/:ref:`& <&>`/g')
else
reppat := $(foreach m,$(cites),-e 's/[[:<:]]$(m)/:ref:`& <&>`/g')
endif

# Delete the line "------" in the first 10 lines of $(DOCS_DIR)/*.rst to
# suppress unnecessary section indices.
$(DOCS_DIR)/%.rst: %.rst
	@$(MKDIR_P) $(DOCS_DIR)
	$(sed_verbose)sed $(reppat) -e '1,10s/^-*$$//' < $< > $@

# remove generated files to be commited (used for `make checkgen`)
clean-local: clean-docs
clean-docs:
	$(rm_verbose)rm -f $(man_in_files) $(doc_files)
