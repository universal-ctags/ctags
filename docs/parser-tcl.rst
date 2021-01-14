.. _tcl:

======================================================================
The new Tcl parser
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

Tcl parser is rewritten as a token oriented parser to support
namespace.  It was line oriented parser. Some incompatibility between
Exuberant Ctags is introduced in the rewriting.

The line oriented parser captures `class`, `public|protected|private
method`.  They are definitions in ITcl and TclOO. The new token oriented Tcl
parser ignores them.  Instead ITcl and TclOO subparser running on Tcl base
parser capture them.

Known bugs
----------------------------------------------------------------------

Full qualified tags
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The separator used in full qualified tags should be `::` but `.` is
used.

A ITcl or TclOO class `C` can be defined in a Tcl namespace `N`:

.. code-block:: Tcl

    namespace eval N {
	oo::class create C {
	}
    }

When ``--extras=+q`` is given, currently ctags reports::

	N.C ...

This should be::

	N::C ...

Much work is needed to fix this.

Nested procs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

`proc` defined in a `proc` cannot be captured well.
This is a regression.
