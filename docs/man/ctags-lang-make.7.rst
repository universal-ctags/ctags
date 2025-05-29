.. _ctags-lang-make(7):

==============================================================
ctags-lang-make
==============================================================

Random notes about tagging Make source code with Universal Ctags

:Version: 6.2.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+Make ...
|	**ctags** ... --language-force=Make ...
|	**ctags** ... --map-Make=+([Mm]akefile) --map-Make=+(GNUmakefile) ...
|	**ctags** ... --map-Make=+.mak --map-Make=+.mk ...

DESCRIPTION
-----------
This parser extracts macro and target definitions. It also extracts included files as references.

Examples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"input.mak"

.. code-block:: Makefile

   -include base.mak

   SRC = hello.c

   all: hello
   hello: hello.o
   hello.o: hello.c
   $(CC) -c $(CFLAGS) $(CPPFLAGS) $<

"output.tags"
with "--options=NONE --extras=+r --fields=+KlrE -o - input.mak"

.. code-block:: tags

   SRC	input.mak	/^SRC = hello.c$/;"	macro	language:Make	roles:def
   all	input.mak	/^all: hello$/;"	target	language:Make	roles:def
   base.mak	input.mak	/^-include base.mak$/;"	makefile	language:Make	roles:optional	extras:reference
   hello	input.mak	/^hello: hello.o$/;"	target	language:Make	roles:def
   hello.o	input.mak	/^hello.o: hello.c$/;"	target	language:Make	roles:def


EXTRACTING CPP MACRO DEFINTIONS DEFINED WITH -DFOO
---------------------------------------------------
With ``-D`` option in a C compiler like gcc, a programmer can define a
macro outside C source files. The options appears on a Makefile
frequently. For an example:

.. code-block:: Makefile

   CPPFLAGS = -DDEBUG

The Make parser has heuristics [DINMAKE]_ for extracting the macros defined with
the option. With enabling ``CppDef`` extra, you can turn on the heuristics.

Examples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

"input.mak"

.. code-block:: Makefile

   -include base.mak

   CFLAGS = -g -O2
   CPPFLAGS = -DOUTPUT=stdout
   SRC = hello.c

   all: hello
   hello: hello.o
   hello.o: hello.c
   $(CC) -c $(CFLAGS) $(CPPFLAGS) $<

"output.tags"
with "--options=NONE --extras-Make=+{CppDef} --fields=+KlE -o - input.mak"

.. code-block:: tags

   CFLAGS	input.mak	/^CFLAGS = -g -O2$/;"	macro	language:Make
   CPPFLAGS	input.mak	/^CPPFLAGS = -DOUTPUT=stdout$/;"	macro	language:Make
   OUTPUT	input.mak	/^CPPFLAGS = -DOUTPUT=stdout$/;"	macro	language:CPreProcessor	extras:CppDef
   SRC	input.mak	/^SRC = hello.c$/;"	macro	language:Make
   all	input.mak	/^all: hello$/;"	target	language:Make
   hello	input.mak	/^hello: hello.o$/;"	target	language:Make
   hello.o	input.mak	/^hello.o: hello.c$/;"	target	language:Make

VERSIONS
--------

Change since "0.0"
~~~~~~~~~~~~~~~~~~

* New extra ``CppDef`` [DINMAKE]_

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`

.. [DINMAKE] `CONFIG_X86_X32_ABI is not visible (defined in Makefile, not C or Kconfig) <https://github.com/bootlin/elixir/issues/221>`_ (https://github.com/bootlin/elixir/issues/221)
