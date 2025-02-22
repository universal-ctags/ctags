#define ENTRY(SYMBOL,LABEL) .global SYMBOL	;\
LABEL: \
	nop

ENTRY(A, B)
ENTRY(C,
	D)
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
