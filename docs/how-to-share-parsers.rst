================================================================================================
Study for sharing code between geany tagmanager and universal-ctags
================================================================================================

:Author: Masatake YAMATO <yamato@redhat.com>

Introduction
================================================

Both geany tagmanager and universal ctags are derived from exuberant
ctags. Currently they are maintained separately.

To utilize developers resource maximally, and minimize maintenance
cost, the merging two code base into one is better. In other hand
geany and u-ctags focus different use case. Completely merging
two into one is not realistic.

In stead of merging, sharing parsers related code between two
projects are realistic. In this document I report my study
about the way for sharing parsers.

Overview
================================================

* Abstraction layer
* Comparison in supported parsers
* Steps and Source code management

Abstraction layer
========================================================


Parser data structure
--------------------------------------------------------

`parserDefinition` is the data structure. Using the same
name in both code. However, u-ctags's one is extended and
is super-set of geany's one.

We have to redefine `parserDefinition` as a common data
structure. In u-ctags, it must be extended like::

  struct uctagsParserDefinition {
	parserDefinition parent;
	...
  };


Interface for invoking a parser
--------------------------------------------------------
The interface for entry points of each parer is the same
between u-ctags and geany.

geany calls a parser very directly like in tagmanager/src/tm_source_file.c::

  LanguageTable [source_file->lang]->parser ();

It seems that ``createTagsForFile`` function in
tagmanager/ctags/parse.c is not used.

parser method doesn't take no argument but it is better to
take the parser itself as an argument for flexibility::

    LanguageTable [source_file->lang]->parser (*LanguageTable [source_file->lang]);

How input is given to a parser
--------------------------------------------------------
See read.h.

Though geany utilizes mio as a abstract input layer,
the input interface for parsers are the same as u-ctags.

u-ctags introduced some extra functions in read.h but they
are for supporting etags TAGS format in xcmd. It is nothing
to do with the input interface for parsers.

Currently input functions don't take an argument specifying
input stream.

We have to care get.c specially.
However, not all the parsers use get.c, so we can start sharing
parsers which don't use get.c.

How to write a tag from a parser
--------------------------------------------------------

Both u-ctags and geany have the same interface between
parser and output layer; a tags is passed to ``makeTagEntry``.

Currently ``makeTagEntry`` just takes one argument, ``TagEntryInfo``,
and doesn't take the output stream or context.

geany introduced output abstraction with a function pointer
named ``TagEntryFunction``. ``makeTagEntry`` calls the
real output function via the pointer. However it is hidden by
``makeTagEntry``. So we have to just look at ``makeTagEntry`` here.

The biggest issue here is about kinds. To keep raw information of
input u-ctags encourages that a parse defines its own kinds suitable
for the target language of the parser. In other hand geany allows a
parser to use language-common kind sets.  geany may use the kind sets
as base for handling multiple languages in the same way.

A layer translating u-ctags kinds to geanys' language-common kind sets
is needed. I named it ``flavor``.

Currently a parser chooses a kind letter and name for a tag which the
parser picks from input stream. This is the bad idea. The choice
is too early. Typical code::

    typedef struct sTagEntryInfo {
    ...
	const char *kindName;	/* kind of tag */
	char	kind;		/* single character representation of kind */


    static void makeBetaTag (const char* const name, const betaKind kind)
    {
	    if (BetaKinds [kind].enabled)
	    {
		    tagEntryInfo e;
		    initTagEntry (&e, name);
		    e.kindName = BetaKinds [kind].name;
		    e.kind     = BetaKinds [kind].letter;
		    makeTagEntry (&e);
	    }
    }


To introduce ``flavor``, following modification is needed::

    typedef struct sKindOption {
	...
    } kindOption;

    typedef struct sTagEntryInfo {
    ...
	const kindOption* kindOption;

    static void makeBetaTag (const char* const name, const betaKind kind)
    {
	    if (BetaKinds [kind].enabled)
	    {
		    tagEntryInfo e;
		    initTagEntry (&e, name);
		    e.kindOption = &(BetaKinds [kind]);
		    makeTagEntry (&e);
	    }
    }


Now ``makeBetaTag`` can receive the ``kindOption`` for a tag.
Next let's add ``flavor*`` slots to ``kindOption``::

    typedef struct sKindOption {
	boolean enabled;			/* are tags for kind enabled? */
	const int letter;			/* kind letter */
	const char* name;			/* kind name */
	const char* const description;	/* displayed in --help output */
	int flavorLetter; /* Initialized with \0 */
	char* flavorName; /* Initialized with NULL */
    } kindOption;

Lower output layer behind ``makeTagEntry`` refers ``flavorLetter``
and ``flavorName`` if they are not 0.

Upper layers of parsers have a response to initialize the flavor
fields before running a parser.  The performance penalty of
introducing flavor is small.


Declaring interface for parsers
--------------------------------------------------------
The interface is the heart of sharing parsers.
It must be maintained together with the both projects.

We may need single header file and common prefix(es) for types,
functions and variables declared in the header file.

``Ctags`` or ``ctags_`` cannot be used. It is reserved for making
ctags a library.


Comparison in supported parsers
========================================================

* Parsers only in u-ctags

  - Ada
  - Ant
  - Asp
  - Awk
  - Basic
  - Clojure
  - DosBatch
  - Eiffel
  - Falcon
  - Flex
  - Lisp
  - MatLab(1)
  - OCaml
  - REXX
  - Scheme
  - SLang
  - SML
  - SystemVerilog
  - Tex(3)
  - Vera
  - VHDL(2)
  - Vim
  - WindRes
  - YACC

* Parsers only in geany

  - Abaqus
  - Abc
  - ActionScript
  - Asciidoc
  - Conf
  - Diff
  - Docbook
  - F77(4)
  - Ferite
  - FreeBasic
  - GLSL
  - Haskell
  - Haxe
  - LaTeX(3)
  - Literate Haskell
  - Markdown
  - Matlab(1)
  - NSIS
  - R
  - Txt2tags
  - Vala
  - Vhdl(2)

Analysis
---------------------------------------------------

u-ctags side
...................................................

We want ctags to be able to use all parsers only available
in geany as far as

* test cases are available, and
* kind definitions are specialized for each language.

Geany side
...................................................

TBW

Misc
...................................................

Both have each own parser for the same language, Matlab/MatLab(1).
u-ctags's one is regex based parser. geany's one is a crated parser.
u-ctags's one tags only functions. geany's one tags functions and structures.
u-ctags's one has a table for 2 gram, which is used in automatic parser
guessing. This table can be portable between two. Maybe geany's one
is better.

Both have each own parser for the same language, VHDL/Vhdl(2).
A maintainer for the language is in u-ctags organization so
we should use u-ctags's one. The maintainer can merge the both
into one.

Though the name is different, TeX and LaTeX parsers may target
the same language(3). u-ctags's one is much larger. geany's one
considers extra extensions ".sty" and ".idx".

F77 parser is special version of Fortran parser(4).

Steps and Source code management
========================================================

We cannot do everything written here at once.
An incremental method is needed.

The both project must introduce ``common-parsers`` and ``own-parsers``
directories. As the name shown, we will share parsers code in
``common-parsers`` directory. Parsers still modifications are needed
should stay in ``own-parsers``. Ideally all ``own-parsers`` is empty
but it will take long time to make it empty.

Of course ``own-parsers`` directory should be maintainer in
each project/git repository. ``common-parsers`` should be
independently maintained. Each project can import the contents of
``common-parsers`` with git submodule.

Current status of u-ctags
----------------------------------------------------------

u-ctags introduced parsers directory at the top level of
source tree. This must be splittedd into ``common-parsers``
and ``own-parsers``.

Current status of geany
----------------------------------------------------------

All codes derived from ctags are stored to geany/tagmanager/ctags.
This should be geany/tagmanager/ctags/common-parsers and
geany/tagmanager/ctags/own-parsers and geany/tagmanager/ctags/main.

Other know issues
================================================

Currently source.mak and parsers.h enumerate all crafted parsers.
When a new parser is added to ``common-parsers`` directory, the files
must be updated. How can we synchronize the update with the parser
addition in the case git repositories are separated.

Each project has its own test harness. When modifying
code in ``common-parsers``, the modification must
be passed test cases in the both project. How
can we implement it in the work-flow of the both
project.

References
================================================

[1] https://github.com/universal-ctags/ctags/issues/63
