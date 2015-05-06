A = 1
B := 2
define C
3
endef
a.o: D = 4
	E = 5
b.o:
	ENV_VAR1=something command
$(obj)/raid6int1.c:   F := 6
default:
	ENV_VAR2=something command
# COMMENT = nada
G = 7 # ignore comment
H = $(A:.y=.c) ${B:.l=.c}
export I = 8
NO_TAG1 += \
-DA=0x00000000L \
-DB=0x00000000L
