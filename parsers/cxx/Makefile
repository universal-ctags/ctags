all:
.SUFFIXES:
.SUFFIXES: .c .o

.c.o:
	$(MAKE) -C ../.. parsers/cxx/$@
%:
	$(MAKE) -C ../.. $@
