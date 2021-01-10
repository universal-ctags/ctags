.. _html:

======================================================================
The new HTML parser
======================================================================

:Maintainer: Jiri Techet <techet@gmail.com>

Introduction
---------------------------------------------------------------------

The old HTML parser was line-oriented based on regular expression matching. This
brought several limitations like the inability of the parser to deal with tags
spanning multiple lines and not respecting HTML comments. In addition, the speed
of the parser depended on the number of regular expressions - the more tag types
were extracted, the more regular expressions were needed and the slower the
parser became. Finally, parsing of embedded JavaScript was very limited, based
on regular expressions and detecting only function declarations.

The new parser is hand-written, using separated lexical analysis (dividing
the input into tokens) and syntax analysis. The parser has been profiled and
optimized for speed so it is one of the fastest parsers in Universal Ctags.
It handles HTML comments correctly and in addition to existing tags it extracts
also <h1>, <h2> and <h3> headings. It should be reasonably simple to add new
tag types.

Finally, the parser uses the new functionality of Universal Ctags to use another
parser for parsing other languages within a host language. This is used for
parsing JavaScript within <script> tags and CSS within <style> tags. This
simplifies the parser and generates much better results than having a simplified
JavaScript or CSS parser within the HTML parser. To run JavaScript and CSS parsers
from HTML parser, use `--extras=+g` option.
