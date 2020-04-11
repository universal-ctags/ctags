all:
.SUFFIXES:
.SUFFIXES: .c .o

.c.o:
	$(MAKE) -C .. readtags-cmd/$@
%:
	$(MAKE) -C .. $@
