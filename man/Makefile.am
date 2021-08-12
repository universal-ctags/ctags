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
	ctags-lang-iPythonCell.7 \
	ctags-lang-julia.7 \
	ctags-lang-python.7 \
	ctags-lang-verilog.7 \
	ctags-lang-inko.7 \
	ctags-lang-r.7 \
	ctags-lang-sql.7 \
	\
	readtags.1 \
	tags.5 \
	\
	$(NULL)

man_pages  = $(GEN_IN_MAN_FILES)
rst_files  = $(addsuffix .rst,$(man_pages))
html_pages = $(addsuffix .html,$(man_pages))
pdf_pages  = $(addsuffix .pdf,$(man_pages))
doc_files  = $(addprefix ../docs/man/,$(rst_files))

# for automake
# generate man pages only when rst2man is installed
if HAVE_RST2MAN
man_MANS = $(man_pages)
endif
EXTRA_DIST = README $(addsuffix .rst.in,$(GEN_IN_MAN_FILES))
CLEANFILES = $(man_pages) $(addsuffix .rst,$(GEN_IN_MAN_FILES)) $(html_pages) $(pdf_pages)

.PHONY: man html pdf update-docs

man: $(man_pages)
html: $(html_pages)
pdf: $(pdf_pages)

#
# Convert the names of man page files (without suffix) to man page citations.
#
# ctags.1 tags.5 ...
# => ctags(1) tags(5) ...
#
cites := ${subst .,(,${addsuffix ),${man_pages}}}

#
# Convert the man page citations to sed patterns for making hyperlinks.
#
# ctags(1) tags(5) ...
# => -e 's/\<ctags(1)/:ref:`& <&>`/g' -e 's/\<ctags(5)/:ref:`& <&>`/g' ...
#
if HAS_GNU_SED
reppat := $(foreach m,$(cites),-e 's/\<$(m)/:ref:`& <&>`/g')
else
reppat := $(foreach m,$(cites),-e 's/[[:<:]]$(m)/:ref:`& <&>`/g')
endif

update-docs: $(doc_files)

# Delete the line "------" in the first 10 lines of ../docs/man/*.rst to
# suppress unnecessary section indices.
../docs/man/%.rst: %.rst
	$(MKDIR_P) ../docs/man
	sed $(reppat) -e '1,10s/^-*$$//' < $< > $@

# use `[@]` not to be replaced by automake
%.rst: %.rst.in
	sed \
		-e s/[@]CTAGS_NAME_EXECUTABLE[@]/ctags/g \
		-e s/[@]ETAGS_NAME_EXECUTABLE[@]/etags/g \
		-e s/[@]VERSION[@]/$(VERSION)/g \
	< $< > $@

%: %.rst
if HAVE_RST2MAN
	$(RST2MAN) $(RST2MAN_FLAGS) $< > $@
else
	$(error Cannot make $@: rst2man is not installed)
endif

%.html: %.rst
if HAVE_RST2HTML
	$(RST2HTML) $(RST2HTML_FLAGS) $< > $@
else
	$(error Cannot make $@: rst2html is not installed)
endif

%.pdf: %.rst
if HAVE_RST2PDF
	$(RST2PDF) $(RST2PDF_FLAGS) $< -o $@
else
	$(error Cannot make $@: rst2pdf is not installed)
endif
