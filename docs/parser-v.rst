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
        sed -n 's/^UNEXPECTED.*at \([^:]*\):.*$/\1/p' | \
        sort | uniq

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

- cross references (except modules)
- function arguments
- closure arguments
- variable types

Design
---------------------------------------------------------------------

The V Parser reads tokens and parses V grammar in parallel (i.e., it does not
build an AST).

The individual grammar parsers all follow these simple rules:
- when called, `token` argument already holds the first token which an
  individual grammar parser should recognise
- on return, individual parser functions read only the tokens they recognises
  and no additional tokens are read (i.e., they do not "over read")
- these rules are enforced by `Assert` statements at the start of each parser
  function

The lexer allows you to "unread" up to `MAX_REPLAYS` tokens.  But unreading a
token only stores it (to be replayed when `getToken()` is next called) and it
does not reset the `token` to hold its previous value.  Where it is necessary to
read ahead and retain the value of a tokens, additional token buffers can be
used (`newToken()`) or the primary token buffer can be duplicated (`dupToken()`)
so that it can continue to be used for reading.  Generally, the primary token
buffer is used where it can be, so that the debug dump accurately shows where
additional buffers are used.  This helps to diagnose situations where unreading
a token does not reset its previous value.

Use of `expectToken()`, rather than `isToken()`, is encouraged where applicable
so that the parser can be run against as much known-good V code as possible and
checked to ensure that is not caught out by uncommon grammar.

Fully-qualified Identifiers and External Symbols
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The following tokens represent identifiers:
- `IDENT` is a V variable/function/field name (e.g., `foo`)
- `TYPE` is a V struct/interface/alias/union name (e.g., `Foo`)
- `EXTERN` is never emitted by the lexer and represents an external symbol

When the lexer returns an `IDENT` or `TYPE` and `parseFullyQualified()` is
subsequently called to consume any additional tokens which may make-up a
fully-qualified identifier, the provided `token` is also updated to reflect the
final, fully-qualified identifier, so that:
- token->string is the whole, fully-qualified name of the identifier (e.g.,
  `user.id`)
- token->type is updated to `IDENT` or `TYPE` to reflect the last part of the
  fully-qualified identifier (e.g., `Foo.bar` is an `IDENT` and `foo.Bar` is a
  `TYPE`), or to `EXTERN` where the fully-qualified identifier is an external
  symbol (e.g., `C.foo` or `JS.Foo`) and the type can not be determined further
- the token is also marked as being fully-qualified, so that subsequent attempts
  to fully-qualify it (e.g., after it is unread and replayed) have no effect
