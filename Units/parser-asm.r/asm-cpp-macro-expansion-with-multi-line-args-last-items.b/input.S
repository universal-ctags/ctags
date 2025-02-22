#define ENTRY(SYMBOL,LABEL) .global SYMBOL	;\
LABEL: \
	nop

ENTRY(
	E,
	F)

ENTRY(
	G,
	H
	)

ENTRY(
	I,
	J,K,
	L
	)
