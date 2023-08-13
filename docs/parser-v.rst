.. _v:

======================================================================
The V parser
======================================================================

:Maintainer: Tim Marston <tim@ed.am>

Development
---------------------------------------------------------------------

The V parser can emit warnings when it encounters code which does not parse.
Normally, this would indicate a problem with the code being parsed.  But for
development, it is useful to run the parser against a ton of known-good code
(e.g., the V sources) to check the parser.  To emit unexpected token warnings,
run ctags with `-d 16`.  (Note: this requires ctags to have been built with
`--enable-debugging`).

A useful terminal command to run the V parser against the whole V source code
and list the names of any files that fail to parser is:

.. code-block:: console

    $ cd vlib
    $ find . -name '*test*' -prune -o -name '*.v' -print0 | \
	    xargs -0 ctags -d 16 2>&1 | \
		awk '{ print $7; }' | grep -v expected | \
		sed -E 's/:[0-9]+//' | sort | uniq

Debugging
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The V parser can also emit a dump of its operation by running ctags with
`--param-v.parserSpew=on`.  (Note: this parser-specific option is also only
available when ctags has been built with `--enable-debugging`.)

The dump is extremely useful for debugging the parser and shows:

- individual grammar parsers starting ({foo:) and ending (:foo})
- lexer reading tokens (UPPERCASE)
  - tokens read in non-primary token buffer appear in square brackets
- tokens being "unread" (˄)
- unread tokens being replayed (˅)
- emitted tags (#)

Shortcomings
---------------------------------------------------------------------

The V parser currently has no support for

- cross references
- function arguments
- closure arguments
