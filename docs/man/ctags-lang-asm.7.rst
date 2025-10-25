.. _ctags-lang-asm(7):

==============================================================
ctags-lang-asm
==============================================================

Random notes about tagging Assembly language source code with Universal Ctags

:Version: 6.2.1
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+Asm ...
|	**ctags** ... --language-force=Asm ...
|	**ctags** ... --map-Asm=+.asm ...
|	**ctags** ... --map-Asm=+.ASM ...
|	**ctags** ... --map-Asm=+.s ...
|	**ctags** ... --map-Asm=+.S ...

DESCRIPTION
-----------
This man page gathers random notes about tagging assembly language
source code.

The parser of Universal Ctags has been extended to support the source
code to be processed with *GNU assembler* (*Gas*).

PARAMETERS
----------
The Asm parser has some parameters for adapting it to different
assembler implementations.

Writing a parser for assembly language source code is not easy because
the syntax for the language differ depending on the implementations of
assemblers and target CPU architectures. For example, in *NASM*, `;`
is a starter of a line comment. On the other hand, in Gas for i386,
`;` is a line separator. The parameters explained this man page are
for mitigating the gaps of syntax.

Use ``--param-Asm.{parameter}={value}`` option for adjusting the value
for a parameter. For example:

.. code-block:: console

	$ ctags ... --param-Asm.runCPreProcessor=false ...

This command line sets ``false`` to ``runCPreProcessor`` parameter.

``--list-params=Asm`` lists available parameters available in the
Asm parser.

``runCPreProcessor``: running C preprocessor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By default, the CPreProcessor parser processes the assembly language
source code before the Asm parser does.

The main effects of running the CPreProcessor parser;

* lines started from `//` are stripped as comments,
* areas surrounded by the pair of `/*` and `*/` are
  stripped as comments, and
* macros defined with `#define` are extracted as tags.

Set ``runCPreProcessor`` to ``false`` for disabling the CPreProcessor
parser running before the Asm parser.

``commentCharsAtBOL``: adjusting line comment starter at the beginning of line
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
By default, the Asm parser ignores lines starting from `;`, `*`, or
`@` as comments. `//` is also ignored if `runCPreProcessor` is `true`.

``commentCharsAtBOL`` is for changing the characters for line comments.
`BOL` is acronym standing for "the beginning of line." The characters
act as comment starters only if they are at the beginning
of lines.

The next example if for assembler input using `!` and `>` as the comment starter:

.. code-block:: console

	$ ctags ... --param-Asm.commentCharsAtBOL='!>' ...

``commentCharsInMOL``: adjusting line comment starter in the middle of line
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some dialects of assemblers support comments starting from the middle of line.
A `;` character starts a comment anywhere on the line in Gas for CRIS for example.

``commentCharsInMOL`` is for specifying the character for line comments.
`MOL` is acronym standing for "the middle of line." Unlike characters
specified with ``commentCharsAtBOL``, the characters specified with
``commentCharsInMOL`` act as comment starts even if they are in the
middle of lines.

By default, the Asm parser has no ``commentCharsInMOL`` characters.

``extraLinesepChars``: adding line separators
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Asm parser processes its input line-oriented way.  By default, the
parser recognizes `\n` as a line separator.  ``extraLinesepChars`` is
for adding more line separators.

In Gas for AArch64, the `;` character can be used as line separators.
The next example for adjusting the Asm parser to the extra line
separator:

.. code-block:: console

	$ ctags ... --param-Asm.extraLinesepChars=';' ...

EXPANDING C PREPROCESSOR MACROS
-------------------------------
The Asm parser has the ability to expand **C preprocessor macros**
before parsing.

.. note::

   Don't confuse C preprocessor macros and assembler implementation
   specific macros. The Asm parser expands only C preprocessor macros.

Specifying following options are must for expansion::

  --param-Asm.runCPreProcessor=true
  --fields=+{signature}
  --fields-CPreProcessor=+{macrodef}

With the above options, the parser expands macros defined in command
line with ``-D`` option. See :ref:`ctags(1) <ctags(1)>` about the way to define a macro
with the ``-D`` option.

With ``--param-CPreProcessor._expand=1`` option, the parser expands
macros defined in the current input file in addition to macros defined
with the ``-D`` option.

Though the parser expands macros, the parser doesn't extract language
objects like labels as you expect. You must adjust the parser specific
parameters to utilize the macro expansion feature effectively. See

An example of macro expansion
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"input.S"

.. code-block::

	#define ENTRY(LABEL) .global LABEL	;\
	LABEL

	ENTRY(main):
		nop

"output.tags"
with "--options=NONE -o - --param-Asm.useCPreProcessor=1 --param-CPreProcessor._expand=1 --fields=+{signature} --fields-CPreProcessor=+{macrodef} --param-Asm.extraLinesepChars=; --fields-CPreProcessor=+{macrodef} input.S"

.. code-block:: tags

	ENTRY	input.S	/^#define ENTRY(/;"	d	file:	signature:(LABEL)	macrodef:.global LABEL ;LABEL
	main	input.S	/^ENTRY(main):$/;"	l
	main	input.S	/^ENTRY(main):$/;"	s

The definition of `ENTRY` assumes `;` is a line separator in the host assembly language.
``--param-Asm.extraLinesepChars=;`` is for satisfying the assumption in ctags side.

Known limitations
~~~~~~~~~~~~~~~~~
The parser has no ability to expand the macros defined outside of the
current input file. The parser doesn't consider `#undef` when
expanding.

VERSIONS
--------

Change since "0.0"
~~~~~~~~~~~~~~~~~~

* The kind ``section`` is deleted.
  The section specified with `.section` directive as tagged as
  ``placement`` role of ``section`` kind of ``Asm`` language.
  These kind and role are deleted.

  Instead, it is tagged as ``destination`` role of ``inputSection``
  kind of ``LdScript`` language.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`,
:ref:`ctags-lang-asm(7) <ctags-lang-asm(7)>`,
Info entries for GNU assembler
