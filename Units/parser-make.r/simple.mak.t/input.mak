A = 1
B := 2
define C
3
endef

# D and E are target specific MAKE variables;
# they are captured as a macro.
a.o: D = 4
	E = 5
# ENV_VAR1 is not a target specific MAKE variables.
# It is a shell variable in a recipe.
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
