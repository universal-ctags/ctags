.. _asm:

======================================================================
Asm parser
======================================================================

.. NOT REVIEWED YET

:Maintainer: Masatake YAMATO <yamato@redhat.com>

The original (Exuberant Ctags) parser handles #define C preprocessor directive and C
style comments by itself. In Universal Ctags Asm parser utilizes CPreProcessor meta
parser for handling them. So a language object defined with #define is tagged as
"defines" of CPreProcessor language, not Asm language.

.. code-block:: console

   $ cat input.S
   #define S 1

   $ e-ctags --fields=+l  -o - input.S
   S	input.S	/^#define S 1$/;"	d	language:Asm

   $ u-ctags --fields=+l  -o - input.S
   S	input.S	/^#define S /;"	d	language:CPreProcessor	file:

