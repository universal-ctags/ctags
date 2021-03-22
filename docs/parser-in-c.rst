.. _writing_parser_in_c:

=============================================================================
Writing a parser in C
=============================================================================

The section is based on the section "Integrating a new language parser" in "`How
to Add Support for a New Language to Exuberant Ctags (EXTENDING)
<http://ctags.sourceforge.net/EXTENDING.html>`_" of Exuberant Ctags documents.

Now suppose that I want to truly integrate compiled-in support for Swine into
ctags.

Registering a parser
-------------------------------------------------
First, I create a new module, ``swine.c``, and add one externally visible function
to it, ``extern parserDefinition *SwineParser(void)``, and add its name to the
table in ``parsers.h``. The job of this parser definition function is to create
an instance of the ``parserDefinition`` structure (using ``parserNew()``) and
populate it with information defining how files of this language are recognized,
what kinds of tags it can locate, and the function used to invoke the parser on
the currently open file.

The structure ``parserDefinition`` allows assignment of the following fields:

.. code-block:: c

	struct sParserDefinition {
		/* defined by parser */
		char* name;                    /* name of language */
		kindDefinition* kindTable;	   /* tag kinds handled by parser */
		unsigned int kindCount;        /* size of 'kinds' list */
		const char *const *extensions; /* list of default extensions */
		const char *const *patterns;   /* list of default file name patterns */
		const char *const *aliases;    /* list of default aliases (alternative names) */
		parserInitialize initialize;   /* initialization routine, if needed */
		parserFinalize finalize;       /* finalize routine, if needed */
		simpleParser parser;           /* simple parser (common case) */
		rescanParser parser2;          /* rescanning parser (unusual case) */
		selectLanguage* selectLanguage; /* may be used to resolve conflicts */
		unsigned int method;           /* See METHOD_ definitions above */
		unsigned int useCork;		   /* bit fields of corkUsage */
		...
	};

The ``name`` field must be set to a non-empty string. Also either ``parser`` or
``parser2`` must set to point to a parsing routine which will generate the tag
entries. All other fields are optional.

Reading input file stream
-------------------------------------------------
Now all that is left is to implement the parser. In order to do its job, the
parser should read the file stream using using one of the two I/O interfaces:
either the character-oriented ``getcFromInputFile()``, or the line-oriented
``readLineFromInputFile()``.

See ":ref:`input-text-stream`" for more details.

Parsing
-------------------------------------------------
How our Swine parser actually parses the contents of the file is entirely up to
the writer of the parser--it can be as crude or elegant as desired. You will
note a variety of examples from the most complex (``parsers/cxx/*.[hc]``) to the
simplest (``parsers/make.[ch]``).

Adding a tag to the tag file
-------------------------------------------------
When the Swine parser identifies an interesting token for which it wants to add
a tag to the tag file, it should create a ``tagEntryInfo`` structure and
initialize it by calling ``initTagEntry()``, which initializes defaults and
fills information about the current line number and the file position of the
beginning of the line. After filling in information defining the current entry
(and possibly overriding the file position or other defaults), the parser passes
this structure to ``makeTagEntry()``.

See ":ref:`output-tag-stream`" for more details.

Adding the parser to ``ctags``
-------------------------------------------------
Lastly, be sure to add your the name of the file containing your parser (e.g.
``parsers/swine.c``) to the macro ``PARSER_SRCS`` in the file ``source.mak``, so
that your new module will be compiled into the program.

Misc.
-------------------------------------------------
This is all there is to it. All other details are specific to the parser and how
it wants to do its job.

There are some support functions which can take care of some commonly needed
parsing tasks, such as *keyword table lookups* (see ``main/keyword.c``), which you
can make use of if desired (examples of its use can be found in ``parsers/c.c``,
``parsers/eiffel.c``, and ``parsers/fortran.c``).

Support functions can be found in ``main/*.h`` excluding ``main/*_p.h``.

Almost everything is already taken care of automatically for you by the
infrastructure. Writing the actual parsing algorithm is the hardest part, but is
not constrained by any need to conform to anything in ctags other than that
mentioned above.

There are several different approaches used in the parsers inside Universal
Ctags and you can browse through these as examples of how to go about creating
your own.
