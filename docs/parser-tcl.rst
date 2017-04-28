.. _tcl:

======================================================================
Tcl parser
======================================================================

:Maintainer: Masatake YAMATO <yamato@redhat.com>

Tcl parser is rewritten as a token oriented parser to support
namespace.  It was line oriented parser. Some incompatibility between
Exuberant-ctags is introduced in the rewriting.

The line oriented parser captures `class`, `public|protected|private
method`.  They are definitions in ITcl and TclOO. The new token oriented Tcl
parser ignores them.  Instead ITcl and TclOO subparser running on Tcl base
parser capture them.
