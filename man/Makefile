#
# This is a stand-alone Makefile for Universal-ctags maintainers.
# GNU Make is needed.
#
# This doesn't work with Makefile.am at the top of source tree.
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
RST2MAN  = rst2man
RST2HTML = rst2html
RST2PDF  = rst2pdf

# rst2man had a bug about code-block:: handling.
# https://sourceforge.net/p/docutils/patches/141
RST2MAN_FLAGS = $(shell if $(RST2MAN) --help | grep -q -e --syntax-highlight; then \
				echo --syntax-highlight=none; \
			fi)
RST2HTML_FLAGS =
RST2PDF_FLAGS =

# To avoid running configure for quick previewing do:
#
# $ make QUICK=1
#
QUICK    =
CONFIGURE_FLAGS =


.SUFFIXES: .rst .html .pdf .in

IN_SOURCE_FILES = \
	ctags.1.rst.in \
	ctags-optlib.7.rst.in \
	ctags-incompatibilities.7.rst.in \
	ctags-client-tools.7.rst.in \
	ctags-faq.7.rst.in \
	\
	ctags-lang-julia.7.rst.in \
	ctags-lang-python.7.rst.in \
	ctags-lang-verilog.7.rst.in \
	ctags-lang-inko.7.rst.in \
	ctags-lang-r.7.rst.in \
	\
	readtags.1.rst.in \
	\
	$(NULL)

SOURCE_FILES = \
	tags.5.rst \
	\
	$(NULL)

gen_rst_files = $(basename $(IN_SOURCE_FILES))
rst_files  = $(gen_rst_files) $(SOURCE_FILES)
man_pages  = $(basename $(rst_files))
html_pages = $(addsuffix .html,$(man_pages))
pdf_pages  = $(addsuffix .pdf,$(man_pages))
doc_files  = $(addprefix ../docs/man/,$(rst_files))

all: man html pdf

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
is_gnu_sed := $(shell sed --version | grep -q GNU && echo 1)
ifeq ($(is_gnu_sed),1)
reppat := $(foreach m,$(cites),-e 's/\<$(m)/:ref:`& <&>`/g')
else
reppat := $(foreach m,$(cites),-e 's/[[:<:]]$(m)/:ref:`& <&>`/g')
endif

update-docs: $(doc_files)

# Delete the line "------" in the first 10 lines of ../docs/man/*.rst to
# suppress unnecessary section indices.
../docs/man/%.rst: %.rst
	sed $(reppat) -e '1,10s/^-*$$//' < $< > $@

%.rst: %.rst.in
ifeq ($(QUICK),)
	(cd ..; ./configure $(CONFIGURE_FLAGS))
else
	version=`sed -n -e 's/AC_INIT(.*, *\[\(.*\)\])/\1/p' ../configure.ac`; \
	sed \
		-e s/@CTAGS_NAME_EXECUTABLE@/ctags/g \
		-e s/@ETAGS_NAME_EXECUTABLE@/etags/g \
		-e s/@VERSION@/$$version/g \
	< $< > $@
endif

%: %.rst
	$(RST2MAN) $(RST2MAN_FLAGS) $< > $@
%.html: %.rst
	$(RST2HTML) $(RST2HTML_FLAGS) $< > $@
%.pdf: %.rst
	$(RST2PDF) $(RST2PDF_FLAGS) $< -o $@

clean:
	rm -f $(gen_rst_files) $(man_pages) $(html_pages) $(pdf_pages)
