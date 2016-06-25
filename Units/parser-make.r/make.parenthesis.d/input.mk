A=X.c
B=Y.c
$(A)-$(A:.c=.o):
	echo $@
${B}-${B:.c=.o}:
	echo $@
