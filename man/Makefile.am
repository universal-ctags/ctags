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

# rst2man and rst2html are part of python-docutils, and if not found
# 	can be installed using pip ("pip install docutils"). On some
#	systems, rst2man and rst2html are actually installed as rst2man.py
#	and rst2html.py - create a symlink of that's the case.
#
# rst2pdf is a separate tool and can also be installed via pip (e.g.,
# 	"pip install rst2pdf")
RST2MAN  = $(shell command -v rst2man  || command -v rst2man.py  || command -v rst2man-3  || command -v rst2man-3.6  || command -v rst2man-3.7  || command -v rst2man-3.8  || command -v rst2man-3.9  || echo rst2man)
RST2HTML = $(shell command -v rst2html || command -v rst2html.py || command -v rst2html-3 || command -v rst2html-3.6 || command -v rst2html-3.7 || command -v rst2html-3.8 || command -v rst2html-3.9 || echo rst2html)
RST2PDF  = rst2pdf

# rst2man had a bug about code-block:: handling.
# https://sourceforge.net/p/docutils/patches/141
RST2MAN_FLAGS = $(shell if $(RST2MAN) --help | grep -q -e --syntax-highlight; then \
				echo --syntax-highlight=none; \
			fi)
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
	\
	$(NULL)

GEN_MAN_FILES = \
	tags.5 \
	\
	$(NULL)

man_pages  = $(GEN_IN_MAN_FILES) $(GEN_MAN_FILES)
rst_files  = $(addsuffix .rst,$(man_pages))
html_pages = $(addsuffix .html,$(man_pages))
pdf_pages  = $(addsuffix .pdf,$(man_pages))
doc_files  = $(addprefix ../docs/man/,$(rst_files))

# for automake
# generate man pages only when rst2man is installed
if HAVE_RST2MAN
man_MANS = $(man_pages)
endif
EXTRA_DIST = README $(addsuffix .rst.in,$(GEN_IN_MAN_FILES)) $(addsuffix .rst,$(GEN_MAN_FILES))
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
	version=`sed -n -e 's/AC_INIT(.*, *\[\(.*\)\])/\1/p' ../configure.ac`; \
	sed \
		-e s/[@]CTAGS_NAME_EXECUTABLE[@]/ctags/g \
		-e s/[@]ETAGS_NAME_EXECUTABLE[@]/etags/g \
		-e s/[@]VERSION[@]/$$version/g \
	< $< > $@

%: %.rst
	$(RST2MAN) $(RST2MAN_FLAGS) $< > $@
%.html: %.rst
	$(RST2HTML) $(RST2HTML_FLAGS) $< > $@
%.pdf: %.rst
	$(RST2PDF) $(RST2PDF_FLAGS) $< -o $@
