.. _vim:

======================================================================
The Vim parser
======================================================================

Incompatible change
---------------------------------------------------------------------

Quoted from ``:help script-variable`` in the Vim documentation::

			    *script-variable* *s:var*
    In a Vim script variables starting with "s:" can be used. They
    cannot be accessed from outside of the scripts, thus are local to
    the script.

Exuberant Ctags records the prefix `s:` as part of a script-local
variable's name. However, it is omitted from function names. As
requested in issue #852 on GitHub, Universal Ctags now also includes
the prefix in script-local function names.
