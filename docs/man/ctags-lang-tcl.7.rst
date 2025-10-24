.. _ctags-lang-tcl(7):

==============================================================
ctags-lang-tcl
==============================================================

Random notes about tagging tcl source code with Universal Ctags

:Version: 6.2.1
:Manual group: Universal Ctags
:Manual section: 7

SYNOPSIS
--------
|	**ctags** ... --languages=+Tcl ...
|	**ctags** ... --language-force=Tcl ...
|	**ctags** ... --map-Tcl=+.tcl ...

DESCRIPTION
-----------
This man page gathers random notes about tagging tcl source code.

TAGGING language objects of OO Extensions
-----------------------------------------

TclOO parser and ITcl parser are subparsers running on the Tcl parser.
As the names of parsers show, they are for tagging language objects of
object oriented programming extensions for the Tcl language.

A pattern, "namespace import oo" in an input file activates the TclOO
parser. A pattern, "namespace import itcl" in an input file activates
the ITcl parser.

There are cases that one of the OO extensions is used though neither
pattern are appeared in an input file.

Consider the following input files:

"main.tcl"

.. code-block:: Tcl

	package require Itcl
	namespace import itcl::*
	source input.tcl

"input.tcl"

.. code-block:: Tcl

	class MyClass {
		public method foo {} {
		}
	}

The pattern for activating the ITcl parser is not appeared
in "input.tcl" though "class" command is used. As a result,
ctags cannot extract "MyClass".

The parameters `TclOO.forceUse=true|[false]` and
`ITcl.forceuse=true|[false]` for handling this situation. With the
parameter, you can force ctags to activate one of the subparsers.

You can use the parameters like ``--param-ITcl.forceuse=true``
in a command-line.

Note that you can enable only one of ITcl parser or TclOO parser.
Enabling both parsers with specifying the parameters can cause
unexpected results.

SEE ALSO
--------
:ref:`ctags(1) <ctags(1)>`
