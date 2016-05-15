.. _vim:

======================================================================
The Vim parser
======================================================================

Incompatible change
---------------------------------------------------------------------

Quoted from help document of vim:

			    *script-variable* *s:var*
    In a Vim script variables starting with "s:" can be used.  They cannot be
    accessed from outside of the scripts, thus are local to the script.

Exuberant-ctags records `s:` as part of variables. However, for function
it doesn't. As requested in #852 on github, Universal-ctags records a
function with `s:` prefix.
