# -*- makefile -*-
.PHONY: help
CTAGS_PROG = $(CTAGS_NAME_EXECUTABLE)

help:
	@echo "Compilation targets:"
	@echo ""
	@echo "make                              - Build $(CTAGS_PROG)"
	@echo "make V=1                          - Build $(CTAGS_PROG) with verbose output"
	@echo "make -f mk_mingw.mak              - Build $(CTAGS_PROG) using MinGW"
	@echo "make -f mk_mingw.mak V=1          - Build $(CTAGS_PROG) using MinGW with verbose output"
	@echo "make -C docs html                 - Build HTML documents by Sphinx"
	@echo ""
	@echo "Testing targets:"
	@echo ""
	@echo "make units                        - Run parser unit test cases"
	@echo "make tmain                        - Run ctags main functionality test cases"
	@echo "make tlib                         - Run mini-geany test cases"
	@echo "make man-test                     - Run testing examples in per-language man pages"
	@echo "make check-genfile                - Run testing generated files are committed"
	@echo "make check                        - Run all tests above"
	@echo ""
	@echo "make fuzz                         - Verify that all parsers are able to properly process each available test unit"
	@echo "make noise                        - Verify the behavior of parsers for broken input: a character injected or removed randomly"
	@echo "make chop                         - Verify the behavior of parsers for broken input: randomly truncated from tail"
	@echo "make slap                         - Verify the behavior of parsers for broken input: randomly truncated from head"
	@echo "make roundtrip                    - Verify the behavior of readtags command"
	@echo
	@echo "Arguments that can be used in testing targets:"
	@echo
	@echo "V=1                               - Verbose output"
	@echo "VG=1                              - Run test cases with Valgrind memory profiler"
	@echo "LANGUAGES=<language>[,<language>] - Only run test cases of the selected languages"
	@echo "CATEGORIES=<category>             - Only run tests available under folder Units/<category>.r"
	@echo "UNITS=<case>[,<case>]             - Only run tests named Units/[category.r/]/<case>.d in units target"
	@echo "                                                         Tmain/<case>.d in tmain target"
	@echo "PMAP=<newlang>/<oldlang>[,...]    - Make <newlang> parser pretend <oldlang> (units target only)"
	@echo ""
	@echo "Input validation target:"
	@echo ""
	@echo "make validate-input               - Validate the input files themselves, not ctags"
	@echo
	@echo "Arguments that can be used in input validation target:"
	@echo "VALIDATORS=<validator>,[<validator>] - Validate only input files expecting validated by VALIDATORs"
