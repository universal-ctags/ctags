.. _python:

======================================================================
The new Python parser
======================================================================

:Maintainer: Colomban Wendling <ban@herbesfolles.org>

Introduction
---------------------------------------------------------------------

The old Python parser was a line-oriented parser that grew way beyond
its capabilities, and ended up riddled with hacks and easily fooled by
perfectly valid input.   By design, it especially had problems dealing
with constructs spanning multiple lines, like triple-quoted strings
or implicitly continued lines; but several less tricky constructs were
also mishandled, and handling of lexical constructs was duplicated and
each clone evolved in its own direction, supporting different features
and having different bugs depending on the location.

All this made it very hard to fix some existing bugs, or add new
features.  To fix this regrettable state of things, the parser has been
rewritten from scratch separating lexical analysis (generating tokens)
from syntactical analysis (understanding what the lexemes mean).
This moves understanding lexemes to a single location, making it
consistent and easier to extend with new lexemes, and lightens the
burden on the parsing code making it more concise, robust and clear.

This rewrite allowed to quite easily fix all known bugs of the old
parser, and add many new features, including:

- Tagging function parameters
- Extraction of decorators
- Proper handling of semicolons
- Extracting multiple variables in a combined declaration
- More accurate support of mixed indentation
- Tagging local variables


The parser should be compatible with the old one.
