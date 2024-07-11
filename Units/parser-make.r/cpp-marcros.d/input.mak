# Derrived from linux/Makefile

REALMODE_CFLAGS	:= -std=gnu11 -m16 -g -Os -DDISABLE_BRANCH_PROFILING -D__DISABLE_EXPORTS \
		   -Wall -Wstrict-prototypes -march=i386 -mregparm=3 \
		   -fno-strict-aliasing -fomit-frame-pointer -fno-pic \
		   -DMYDEF=1 \
		   -DMYDEF0   \
		   -DMYDEF1=1 \
		   -DMYDEF2(X)=X   \
		   -D MYDEF3      \
		   -D MYDEF4=1    \
		   -D MYDEF5(X)=X \
		   -D'MYDEF6'      \
		   -D'MYDEF7=1'    \
		   -D'MYDEF8(X)=X' \
		   -D 'MYDEF9'      \
		   -D 'MYDEFa=1'    \
		   -D 'MYDEFb(X)=X' \
		   -D"MYDEFc"      \
		   -D"MYDEFd=1"    \
		   -D"MYDEFe(X)=X" \
		   -D "MYDEFf"      \
		   -D "MYDEFg=1"    \
		   -D "MYDEFh(X)=X" \
		   -mno-mmx -mno-sse $(call cc-option,-fcf-protection=none)

KBUILD_CPPFLAGS := -D__KERNEL__
KBUILD_RUSTFLAGS := $(rust_common_flags) \
		    -Cpanic=abort -Cembed-bitcode=n -Clto=n \
		    -Cforce-unwind-tables=n -Ccodegen-units=1 \
		    -Csymbol-mangling-version=v0 \
		    -Crelocation-model=static \
		    -Zfunction-sections=n \
		    -Dclippy::float_arithmetic

KBUILD_AFLAGS_KERNEL :=
KBUILD_CFLAGS_KERNEL :=
KBUILD_RUSTFLAGS_KERNEL :=
KBUILD_AFLAGS_MODULE  := -DMODULE
KBUILD_CFLAGS_MODULE  := -DMODULE
KBUILD_RUSTFLAGS_MODULE := --cfg MODULE

all:
	$(CC) -DFOO ...

define macdef
       MACDEF_CPPFLAGS  = -DMACDEFMAC0(X0,Y0,Z0)
       MACDEF_CPPFLAGS += -DMACDEFMAC1(X1,Y1,Z1)=X1##Y1##Z1
       MACDEF_CPPFLAGS_BROKEN0 = -DMACDEFMAC_BROKEN0(A0,B0,C0
       MACDEF_CPPFLAGS_BROKEN1 = -D MACDEFMAC_BROKEN1(A1,B1,C1
       MACDEF_CPPFLAGS += -DMACDEFMAC2(X2,Y2,Z2)="f b"
endif
