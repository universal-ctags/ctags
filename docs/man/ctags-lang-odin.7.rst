.. _ctags-lang-odin(7):

==============================================================
ctags-lang-odin
==============================================================

Notes about tagging Odin source code with Universal Ctags

:Version: 6.2.0
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+Odin ...
|	**ctags** ... --language-force=Odin ...
|	**ctags** ... --map-Odin=+.odin ...

DESCRIPTION
-----------
This man page gathers notes about tagging Odin source code
with Universal Ctags.

The Odin parser tags packages, procedures, constants, variables,
structs, enums, unions, struct members, enum values, type aliases,
and foreign imports.

Distinguishing types from constants
------------------------------------
In Odin, the syntax for a type alias and a constant binding are
identical::

	My_Type :: Other_Type
	MY_CONST :: OTHER_CONST

When the right-hand side of a ``::`` binding is a plain identifier,
the parser cannot syntactically determine whether it is a type alias
or a constant. In this case, the parser uses a naming-convention
heuristic: if the name on the left-hand side contains any lowercase
letter, it is tagged as a ``type`` (kind ``t``); if the name is
entirely uppercase (and underscores/digits), it is tagged as a
``const`` (kind ``c``).

This follows the strong Odin convention that type names use
``Ada_Case`` and constants use ``SCREAMING_SNAKE_CASE``.

"input.odin"

.. code-block:: Odin

	package example

	Handle :: distinct rawptr
	Vector2 :: [2]f32
	Callback :: proc(x: int) -> bool
	My_Alias :: Some_Type
	MAX_SIZE :: SOME_VALUE

"output.tags"
with "--options=NONE --sort=no --fields=+K -o - input.odin"

.. code-block:: tags

	example	input.odin	/^package example$/;"	package
	Handle	input.odin	/^Handle :: distinct rawptr$/;"	type	package:example
	Vector2	input.odin	/^Vector2 :: [2]f32$/;"	type	package:example
	Callback	input.odin	/^Callback :: proc(x: int) -> bool$/;"	type	package:example
	My_Alias	input.odin	/^My_Alias :: Some_Type$/;"	type	package:example
	MAX_SIZE	input.odin	/^MAX_SIZE :: SOME_VALUE$/;"	const	package:example

Note that when the right-hand side is a keyword such as ``struct``,
``enum``, ``union``, ``distinct``, ``proc``, ``map``, or a syntactic
form such as ``^Type``, ``[N]Type``, or ``#type``, the parser
identifies the kind unambiguously without relying on the heuristic.

The heuristic will misclassify names that break the convention, such
as an all-uppercase type alias (``HANDLE :: Some_Type`` would be
tagged as ``const``) or a lowercase-containing constant
(``kMaxSize :: SOME_VALUE`` would be tagged as ``type``).
In practice, these cases are rare in idiomatic Odin code.

Foreign blocks
--------------
The parser extracts procedure and variable declarations from
``foreign`` blocks.

"input.odin"

.. code-block:: Odin

	package ffi

	foreign import libc "system:libc.so"

	foreign libc {
	    puts :: proc "c" (s: cstring) -> i32 ---
	    errno: i32
	}

"output.tags"
with "--options=NONE --sort=no --fields=+K -o - input.odin"

.. code-block:: tags

	ffi	input.odin	/^package ffi$/;"	package
	libc	input.odin	/^foreign import libc "system:libc.so"$/;"	foreign	package:ffi
	puts	input.odin	/^    puts :: proc "c" (s: cstring) -> i32 ---$/;"	proc	package:ffi
	errno	input.odin	/^    errno: i32$/;"	var	package:ffi

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`, :ref:`ctags-client-tools(7) <ctags-client-tools(7)>`
