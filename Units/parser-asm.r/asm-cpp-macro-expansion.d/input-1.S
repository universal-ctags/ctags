#define ENTRY1(LABEL) .global LABEL	;\
LABEL

#comment
ENTRY1(dummy1:
	nop

ENTRY1(exit1):
	nop
