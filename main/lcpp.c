/*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains the high level input read functions (preprocessor
*   directives are handled within this level).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "debug.h"
#include "entry.h"
#include "lcpp.h"
#include "kind.h"
#include "options.h"
#include "read.h"
#include "vstring.h"
#include "parse.h"
#include "xtag.h"

/*
*   MACROS
*/
#define stringMatch(s1,s2)		(strcmp (s1,s2) == 0)
#define isspacetab(c)			((c) == SPACE || (c) == TAB)

/*
*   DATA DECLARATIONS
*/
typedef enum { COMMENT_NONE, COMMENT_C, COMMENT_CPLUS, COMMENT_D } Comment;

enum eCppLimits {
	MaxCppNestingLevel = 20,
	MaxDirectiveName = 10
};

/*  Defines the one nesting level of a preprocessor conditional.
 */
typedef struct sConditionalInfo {
	boolean ignoreAllBranches;  /* ignoring parent conditional branch */
	boolean singleBranch;       /* choose only one branch */
	boolean branchChosen;       /* branch already selected */
	boolean ignoring;           /* current ignore state */
} conditionalInfo;

enum eState {
	DRCTV_NONE,    /* no known directive - ignore to end of line */
	DRCTV_DEFINE,  /* "#define" encountered */
	DRCTV_HASH,    /* initial '#' read; determine directive */
	DRCTV_IF,      /* "#if" or "#ifdef" encountered */
	DRCTV_PRAGMA,  /* #pragma encountered */
	DRCTV_UNDEF,   /* "#undef" encountered */
	DRCTV_INCLUDE, /* "#include" encountered */
};

/*  Defines the current state of the pre-processor.
 */
typedef struct sCppState {
	int		ungetch, ungetch2;   /* ungotten characters, if any */
	boolean resolveRequired;     /* must resolve if/else/elif/endif branch */
	boolean hasAtLiteralStrings; /* supports @"c:\" strings */
	boolean hasCxxRawLiteralStrings; /* supports R"xxx(...)xxx" strings */
	boolean hasSingleQuoteLiteralNumbers; /* supports vera number literals:
						 'h..., 'o..., 'd..., and 'b... */
	const kindOption  *defineMacroKind;
	int macroUndefRoleIndex;
	const kindOption  *headerKind;
	int headerSystemRoleIndex;
	int headerLocalRoleIndex;

	struct sDirective {
		enum eState state;       /* current directive being processed */
		boolean	accept;          /* is a directive syntactically permitted? */
		vString * name;          /* macro name */
		unsigned int nestLevel;  /* level 0 is not used */
		conditionalInfo ifdef [MaxCppNestingLevel];
	} directive;
} cppState;

/*
*   DATA DEFINITIONS
*/

/*  Use brace formatting to detect end of block.
 */
static boolean BraceFormat = FALSE;

static cppState Cpp = {
	'\0', '\0',  /* ungetch characters */
	FALSE,       /* resolveRequired */
	FALSE,       /* hasAtLiteralStrings */
	FALSE,       /* hasCxxRawLiteralStrings */
	FALSE,	     /* hasSingleQuoteLiteralNumbers */
	NULL,	     /* defineMacroKind */
	.macroUndefRoleIndex   = ROLE_INDEX_DEFINITION,
	NULL,	     /* headerKind */
	.headerSystemRoleIndex = ROLE_INDEX_DEFINITION,
	.headerLocalRoleIndex = ROLE_INDEX_DEFINITION,
	{
		DRCTV_NONE,  /* state */
		FALSE,       /* accept */
		NULL,        /* tag name */
		0,           /* nestLevel */
		{ {FALSE,FALSE,FALSE,FALSE} }  /* ifdef array */
	}  /* directive */
};

/*
*   FUNCTION DEFINITIONS
*/

extern boolean cppIsBraceFormat (void)
{
	return BraceFormat;
}

extern unsigned int cppGetDirectiveNestLevel (void)
{
	return Cpp.directive.nestLevel;
}

extern void cppInit (const boolean state, const boolean hasAtLiteralStrings,
		     const boolean hasCxxRawLiteralStrings,
		     const boolean hasSingleQuoteLiteralNumbers,
		     const kindOption *defineMacroKind,
		     int macroUndefRoleIndex,
		     const kindOption *headerKind,
		     int headerSystemRoleIndex, int headerLocalRoleIndex)
{
	BraceFormat = state;

	Cpp.ungetch         = '\0';
	Cpp.ungetch2        = '\0';
	Cpp.resolveRequired = FALSE;
	Cpp.hasAtLiteralStrings = hasAtLiteralStrings;
	Cpp.hasCxxRawLiteralStrings = hasCxxRawLiteralStrings;
	Cpp.hasSingleQuoteLiteralNumbers = hasSingleQuoteLiteralNumbers;
	Cpp.defineMacroKind  = defineMacroKind;
	Cpp.macroUndefRoleIndex = macroUndefRoleIndex;
	Cpp.headerKind  = headerKind;
	Cpp.headerSystemRoleIndex = headerSystemRoleIndex;
	Cpp.headerLocalRoleIndex = headerLocalRoleIndex;

	Cpp.directive.state     = DRCTV_NONE;
	Cpp.directive.accept    = TRUE;
	Cpp.directive.nestLevel = 0;

	Cpp.directive.ifdef [0].ignoreAllBranches = FALSE;
	Cpp.directive.ifdef [0].singleBranch = FALSE;
	Cpp.directive.ifdef [0].branchChosen = FALSE;
	Cpp.directive.ifdef [0].ignoring     = FALSE;

	Cpp.directive.name = vStringNewOrClear (Cpp.directive.name);
}

extern void cppTerminate (void)
{
	if (Cpp.directive.name != NULL)
	{
		vStringDelete (Cpp.directive.name);
		Cpp.directive.name = NULL;
	}
}

extern void cppBeginStatement (void)
{
	Cpp.resolveRequired = TRUE;
}

extern void cppEndStatement (void)
{
	Cpp.resolveRequired = FALSE;
}

/*
*   Scanning functions
*
*   This section handles preprocessor directives.  It strips out all
*   directives and may emit a tag for #define directives.
*/

/*  This puts a character back into the input queue for the input File.
 *  Up to two characters may be ungotten.
 */
extern void cppUngetc (const int c)
{
	Assert (Cpp.ungetch2 == '\0');
	Cpp.ungetch2 = Cpp.ungetch;
	Cpp.ungetch = c;
}

/*  Reads a directive, whose first character is given by "c", into "name".
 */
static boolean readDirective (int c, char *const name, unsigned int maxLength)
{
	unsigned int i;

	for (i = 0  ;  i < maxLength - 1  ;  ++i)
	{
		if (i > 0)
		{
			c = getcFromInputFile ();
			if (c == EOF  ||  ! isalpha (c))
			{
				ungetcToInputFile (c);
				break;
			}
		}
		name [i] = c;
	}
	name [i] = '\0';  /* null terminate */

	return (boolean) isspacetab (c);
}

/*  Reads an identifier, whose first character is given by "c", into "tag",
 *  together with the file location and corresponding line number.
 */
static void readIdentifier (int c, vString *const name)
{
	vStringClear (name);
	do
	{
		vStringPut (name, c);
		c = getcFromInputFile ();
	} while (c != EOF  && cppIsident (c));
	ungetcToInputFile (c);
	vStringTerminate (name);
}

static void readFilename (int c, vString *const name)
{
	int c_end = (c == '<')? '>': '"';

	vStringClear (name);

	while (c = getcFromInputFile (), (c != EOF && c != c_end && c != '\n'))
		vStringPut (name, c);

	vStringTerminate (name);
}

static conditionalInfo *currentConditional (void)
{
	return &Cpp.directive.ifdef [Cpp.directive.nestLevel];
}

static boolean isIgnore (void)
{
	return Cpp.directive.ifdef [Cpp.directive.nestLevel].ignoring;
}

static boolean setIgnore (const boolean ignore)
{
	return Cpp.directive.ifdef [Cpp.directive.nestLevel].ignoring = ignore;
}

static boolean isIgnoreBranch (void)
{
	conditionalInfo *const ifdef = currentConditional ();

	/*  Force a single branch if an incomplete statement is discovered
	 *  en route. This may have allowed earlier branches containing complete
	 *  statements to be followed, but we must follow no further branches.
	 */
	if (Cpp.resolveRequired  &&  ! BraceFormat)
		ifdef->singleBranch = TRUE;

	/*  We will ignore this branch in the following cases:
	 *
	 *  1.  We are ignoring all branches (conditional was within an ignored
	 *        branch of the parent conditional)
	 *  2.  A branch has already been chosen and either of:
	 *      a.  A statement was incomplete upon entering the conditional
	 *      b.  A statement is incomplete upon encountering a branch
	 */
	return (boolean) (ifdef->ignoreAllBranches ||
					 (ifdef->branchChosen  &&  ifdef->singleBranch));
}

static void chooseBranch (void)
{
	if (! BraceFormat)
	{
		conditionalInfo *const ifdef = currentConditional ();

		ifdef->branchChosen = (boolean) (ifdef->singleBranch ||
										Cpp.resolveRequired);
	}
}

/*  Pushes one nesting level for an #if directive, indicating whether or not
 *  the branch should be ignored and whether a branch has already been chosen.
 */
static boolean pushConditional (const boolean firstBranchChosen)
{
	const boolean ignoreAllBranches = isIgnore ();  /* current ignore */
	boolean ignoreBranch = FALSE;

	if (Cpp.directive.nestLevel < (unsigned int) MaxCppNestingLevel - 1)
	{
		conditionalInfo *ifdef;

		++Cpp.directive.nestLevel;
		ifdef = currentConditional ();

		/*  We take a snapshot of whether there is an incomplete statement in
		 *  progress upon encountering the preprocessor conditional. If so,
		 *  then we will flag that only a single branch of the conditional
		 *  should be followed.
		 */
		ifdef->ignoreAllBranches = ignoreAllBranches;
		ifdef->singleBranch      = Cpp.resolveRequired;
		ifdef->branchChosen      = firstBranchChosen;
		ifdef->ignoring = (boolean) (ignoreAllBranches || (
				! firstBranchChosen  &&  ! BraceFormat  &&
				(ifdef->singleBranch || !Option.if0)));
		ignoreBranch = ifdef->ignoring;
	}
	return ignoreBranch;
}

/*  Pops one nesting level for an #endif directive.
 */
static boolean popConditional (void)
{
	if (Cpp.directive.nestLevel > 0)
	{
		--Cpp.directive.nestLevel;
	}

	return isIgnore ();
}


static int makeDefineTag (const char *const name, const char* const signature, boolean undef)
{
	const boolean isFileScope = (boolean) (! isInputHeaderFile ());

	if (!Cpp.defineMacroKind)
		return CORK_NIL;
	if (isFileScope && !isXtagEnabled(XTAG_FILE_SCOPE))
		return CORK_NIL;

	if (Cpp.macroUndefRoleIndex == ROLE_INDEX_DEFINITION)
		return CORK_NIL;

	if ( /* condition for definition tag */
		((!undef) && Cpp.defineMacroKind->enabled)
		|| /* condition for reference tag */
		(undef && isXtagEnabled(XTAG_REFERENCE_TAGS) &&
		 Cpp.defineMacroKind->roles [ Cpp.macroUndefRoleIndex ].enabled))
	{
		tagEntryInfo e;

		if (undef)
			initRefTagEntry (&e, name, Cpp.defineMacroKind,
					 Cpp.macroUndefRoleIndex);
		else
			initTagEntry (&e, name, Cpp.defineMacroKind);
		e.lineNumberEntry = (boolean) (Option.locate == EX_LINENUM);
		e.isFileScope  = isFileScope;
		if (isFileScope)
			markTagExtraBit (&e, XTAG_FILE_SCOPE);
		e.truncateLine = TRUE;
		e.extensionFields.signature = signature;
		return makeTagEntry (&e);
	}
	return CORK_NIL;
}

static void makeIncludeTag (const  char *const name, boolean systemHeader)
{
	tagEntryInfo e;
	int role_index = systemHeader? Cpp.headerSystemRoleIndex: Cpp.headerLocalRoleIndex;

	if (role_index == ROLE_INDEX_DEFINITION)
		return;

	if (Cpp.headerKind && Cpp.headerKind->enabled
	    && isXtagEnabled (XTAG_REFERENCE_TAGS)
	    && Cpp.headerKind->roles [ role_index ].enabled)
	{
		initRefTagEntry (&e, name, Cpp.headerKind, role_index);
		e.lineNumberEntry = (boolean) (Option.locate == EX_LINENUM);
		e.isFileScope  = FALSE;
		e.truncateLine = TRUE;
		makeTagEntry (&e);
	}
}

static vString *signature;
static int directiveDefine (const int c, boolean undef)
{
	int r = CORK_NIL;

	if (cppIsident1 (c))
	{
		readIdentifier (c, Cpp.directive.name);
		if (! isIgnore ())
		{
			int p;

			p = getcFromInputFile ();
			if (p == '(')
			{
				signature = vStringNewOrClear (signature);
				do {
					if (!isspacetab(p))
						vStringPut (signature, p);
					/* TODO: Macro parameters can be captured here. */
					p = getcFromInputFile ();
				} while (p != ')' && p != EOF);

				if (p == ')')
				{
					vStringPut (signature, p);
					r = makeDefineTag (vStringValue (Cpp.directive.name), vStringValue (signature), undef);
				}
				else
					r = makeDefineTag (vStringValue (Cpp.directive.name), NULL, undef);
			}
			else
			{
				ungetcToInputFile (p);
				r = makeDefineTag (vStringValue (Cpp.directive.name), NULL, undef);
			}
		}
	}
	Cpp.directive.state = DRCTV_NONE;
	return r;
}

static void directiveUndef (const int c)
{
	if (isXtagEnabled (XTAG_REFERENCE_TAGS))
	{
		directiveDefine (c, TRUE);
	}
	else
	{
		Cpp.directive.state = DRCTV_NONE;
	}
}

static void directivePragma (int c)
{
	if (cppIsident1 (c))
	{
		readIdentifier (c, Cpp.directive.name);
		if (stringMatch (vStringValue (Cpp.directive.name), "weak"))
		{
			/* generate macro tag for weak name */
			do
			{
				c = getcFromInputFile ();
			} while (c == SPACE);
			if (cppIsident1 (c))
			{
				readIdentifier (c, Cpp.directive.name);
				makeDefineTag (vStringValue (Cpp.directive.name), NULL, FALSE);
			}
		}
	}
	Cpp.directive.state = DRCTV_NONE;
}

static boolean directiveIf (const int c)
{
	DebugStatement ( const boolean ignore0 = isIgnore (); )
	const boolean ignore = pushConditional ((boolean) (c != '0'));

	Cpp.directive.state = DRCTV_NONE;
	DebugStatement ( debugCppNest (TRUE, Cpp.directive.nestLevel);
	                 if (ignore != ignore0) debugCppIgnore (ignore); )

	return ignore;
}


static void directiveInclude (const int c)
{
	if (c == '<' || c == '"')
	{
		readFilename (c, Cpp.directive.name);
		if ((! isIgnore ()) && vStringLength (Cpp.directive.name))
			makeIncludeTag (vStringValue (Cpp.directive.name),
					c == '<');
	}
	Cpp.directive.state = DRCTV_NONE;
}

static boolean directiveHash (const int c)
{
	boolean ignore = FALSE;
	char directive [MaxDirectiveName];
	DebugStatement ( const boolean ignore0 = isIgnore (); )

	readDirective (c, directive, MaxDirectiveName);
	if (stringMatch (directive, "define"))
		Cpp.directive.state = DRCTV_DEFINE;
	else if (stringMatch (directive, "include"))
		Cpp.directive.state = DRCTV_INCLUDE;
	else if (stringMatch (directive, "undef"))
		Cpp.directive.state = DRCTV_UNDEF;
	else if (strncmp (directive, "if", (size_t) 2) == 0)
		Cpp.directive.state = DRCTV_IF;
	else if (stringMatch (directive, "elif")  ||
			stringMatch (directive, "else"))
	{
		ignore = setIgnore (isIgnoreBranch ());
		if (! ignore  &&  stringMatch (directive, "else"))
			chooseBranch ();
		Cpp.directive.state = DRCTV_NONE;
		DebugStatement ( if (ignore != ignore0) debugCppIgnore (ignore); )
	}
	else if (stringMatch (directive, "endif"))
	{
		DebugStatement ( debugCppNest (FALSE, Cpp.directive.nestLevel); )
		ignore = popConditional ();
		Cpp.directive.state = DRCTV_NONE;
		DebugStatement ( if (ignore != ignore0) debugCppIgnore (ignore); )
	}
	else if (stringMatch (directive, "pragma"))
		Cpp.directive.state = DRCTV_PRAGMA;
	else
		Cpp.directive.state = DRCTV_NONE;

	return ignore;
}

/*  Handles a pre-processor directive whose first character is given by "c".
 */
static boolean handleDirective (const int c, int *macroCorkIndex)
{
	boolean ignore = isIgnore ();

	switch (Cpp.directive.state)
	{
		case DRCTV_NONE:    ignore = isIgnore ();        break;
		case DRCTV_DEFINE:
			*macroCorkIndex = directiveDefine (c, FALSE);
			break;
		case DRCTV_HASH:    ignore = directiveHash (c);  break;
		case DRCTV_IF:      ignore = directiveIf (c);    break;
		case DRCTV_PRAGMA:  directivePragma (c);         break;
		case DRCTV_UNDEF:   directiveUndef (c);         break;
		case DRCTV_INCLUDE: directiveInclude (c);         break;
	}
	return ignore;
}

/*  Called upon reading of a slash ('/') characters, determines whether a
 *  comment is encountered, and its type.
 */
static Comment isComment (void)
{
	Comment comment;
	const int next = getcFromInputFile ();

	if (next == '*')
		comment = COMMENT_C;
	else if (next == '/')
		comment = COMMENT_CPLUS;
	else if (next == '+')
		comment = COMMENT_D;
	else
	{
		ungetcToInputFile (next);
		comment = COMMENT_NONE;
	}
	return comment;
}

/*  Skips over a C style comment. According to ANSI specification a comment
 *  is treated as white space, so we perform this substitution.
 */
int cppSkipOverCComment (void)
{
	int c = getcFromInputFile ();

	while (c != EOF)
	{
		if (c != '*')
			c = getcFromInputFile ();
		else
		{
			const int next = getcFromInputFile ();

			if (next != '/')
				c = next;
			else
			{
				c = SPACE;  /* replace comment with space */
				break;
			}
		}
	}
	return c;
}

/*  Skips over a C++ style comment.
 */
static int skipOverCplusComment (void)
{
	int c;

	while ((c = getcFromInputFile ()) != EOF)
	{
		if (c == BACKSLASH)
			getcFromInputFile ();  /* throw away next character, too */
		else if (c == NEWLINE)
			break;
	}
	return c;
}

/* Skips over a D style comment.
 * Really we should match nested /+ comments. At least they're less common.
 */
static int skipOverDComment (void)
{
	int c = getcFromInputFile ();

	while (c != EOF)
	{
		if (c != '+')
			c = getcFromInputFile ();
		else
		{
			const int next = getcFromInputFile ();

			if (next != '/')
				c = next;
			else
			{
				c = SPACE;  /* replace comment with space */
				break;
			}
		}
	}
	return c;
}

/*  Skips to the end of a string, returning a special character to
 *  symbolically represent a generic string.
 */
static int skipToEndOfString (boolean ignoreBackslash)
{
	int c;

	while ((c = getcFromInputFile ()) != EOF)
	{
		if (c == BACKSLASH && ! ignoreBackslash)
			getcFromInputFile ();  /* throw away next character, too */
		else if (c == DOUBLE_QUOTE)
			break;
	}
	return STRING_SYMBOL;  /* symbolic representation of string */
}


static int isCxxRawLiteralDelimiterChar (int c)
{
	return (c != ' ' && c != '\f' && c != '\n' && c != '\r' && c != '\t' && c != '\v' &&
	        c != '(' && c != ')' && c != '\\');
}

static int skipToEndOfCxxRawLiteralString (void)
{
	int c = getcFromInputFile ();

	if (c != '(' && ! isCxxRawLiteralDelimiterChar (c))
	{
		ungetcToInputFile (c);
		c = skipToEndOfString (FALSE);
	}
	else
	{
		char delim[16];
		unsigned int delimLen = 0;
		boolean collectDelim = TRUE;

		do
		{
			if (collectDelim)
			{
				if (isCxxRawLiteralDelimiterChar (c) &&
				    delimLen < (sizeof delim / sizeof *delim))
					delim[delimLen++] = c;
				else
					collectDelim = FALSE;
			}
			else if (c == ')')
			{
				unsigned int i = 0;

				while ((c = getcFromInputFile ()) != EOF && i < delimLen && delim[i] == c)
					i++;
				if (i == delimLen && c == DOUBLE_QUOTE)
					break;
				else
					ungetcToInputFile (c);
			}
		}
		while ((c = getcFromInputFile ()) != EOF);
		c = STRING_SYMBOL;
	}
	return c;
}

/*  Skips to the end of the three (possibly four) 'c' sequence, returning a
 *  special character to symbolically represent a generic character.
 *  Also detects Vera numbers that include a base specifier (ie. 'b1010).
 */
static int skipToEndOfChar (void)
{
	int c;
	int count = 0, veraBase = '\0';

	while ((c = getcFromInputFile ()) != EOF)
	{
	    ++count;
		if (c == BACKSLASH)
			getcFromInputFile ();  /* throw away next character, too */
		else if (c == SINGLE_QUOTE)
			break;
		else if (c == NEWLINE)
		{
			ungetcToInputFile (c);
			break;
		}
		else if (Cpp.hasSingleQuoteLiteralNumbers)
		{
			if (count == 1  &&  strchr ("DHOB", toupper (c)) != NULL)
				veraBase = c;
			else if (veraBase != '\0'  &&  ! isalnum (c))
			{
				ungetcToInputFile (c);
				break;
			}
		}
	}
	return CHAR_SYMBOL;  /* symbolic representation of character */
}

static void attachEndFieldMaybe (int macroCorkIndex)
{
	if (macroCorkIndex != CORK_NIL)
	{
		tagEntryInfo *tag;

		tag = getEntryInCorkQueue (macroCorkIndex);
		tag->extensionFields.endLine = getInputLineNumber ();
	}
}

/*  This function returns the next character, stripping out comments,
 *  C pre-processor directives, and the contents of single and double
 *  quoted strings. In short, strip anything which places a burden upon
 *  the tokenizer.
 */
extern int cppGetc (void)
{
	boolean directive = FALSE;
	boolean ignore = FALSE;
	int c;
	int macroCorkIndex = CORK_NIL;

	if (Cpp.ungetch != '\0')
	{
		c = Cpp.ungetch;
		Cpp.ungetch = Cpp.ungetch2;
		Cpp.ungetch2 = '\0';
		return c;  /* return here to avoid re-calling debugPutc () */
	}
	else do
	{
start_loop:
		c = getcFromInputFile ();
process:
		switch (c)
		{
			case EOF:
				ignore    = FALSE;
				directive = FALSE;
				attachEndFieldMaybe (macroCorkIndex);
				macroCorkIndex = CORK_NIL;
				break;

			case TAB:
			case SPACE:
				break;  /* ignore most white space */

			case NEWLINE:
				if (directive  &&  ! ignore)
				{
					attachEndFieldMaybe (macroCorkIndex);
					macroCorkIndex = CORK_NIL;
					directive = FALSE;
				}
				Cpp.directive.accept = TRUE;
				break;

			case DOUBLE_QUOTE:
				if (Cpp.directive.state == DRCTV_INCLUDE)
					goto enter;
				else
				{
					Cpp.directive.accept = FALSE;
					c = skipToEndOfString (FALSE);
				}

				break;

			case '#':
				if (Cpp.directive.accept)
				{
					directive = TRUE;
					Cpp.directive.state  = DRCTV_HASH;
					Cpp.directive.accept = FALSE;
				}
				break;

			case SINGLE_QUOTE:
				Cpp.directive.accept = FALSE;
				c = skipToEndOfChar ();
				break;

			case '/':
			{
				const Comment comment = isComment ();

				if (comment == COMMENT_C)
					c = cppSkipOverCComment ();
				else if (comment == COMMENT_CPLUS)
				{
					c = skipOverCplusComment ();
					if (c == NEWLINE)
						ungetcToInputFile (c);
				}
				else if (comment == COMMENT_D)
					c = skipOverDComment ();
				else
					Cpp.directive.accept = FALSE;
				break;
			}

			case BACKSLASH:
			{
				int next = getcFromInputFile ();

				if (next == NEWLINE)
					goto start_loop;
				else
					ungetcToInputFile (next);
				break;
			}

			case '?':
			{
				int next = getcFromInputFile ();
				if (next != '?')
					ungetcToInputFile (next);
				else
				{
					next = getcFromInputFile ();
					switch (next)
					{
						case '(':          c = '[';       break;
						case ')':          c = ']';       break;
						case '<':          c = '{';       break;
						case '>':          c = '}';       break;
						case '/':          c = BACKSLASH; goto process;
						case '!':          c = '|';       break;
						case SINGLE_QUOTE: c = '^';       break;
						case '-':          c = '~';       break;
						case '=':          c = '#';       goto process;
						default:
							ungetcToInputFile ('?');
							ungetcToInputFile (next);
							break;
					}
				}
			} break;

			/* digraphs:
			 * input:  <:  :>  <%  %>  %:  %:%:
			 * output: [   ]   {   }   #   ##
			 */
			case '<':
			{
				int next = getcFromInputFile ();
				switch (next)
				{
					case ':':	c = '['; break;
					case '%':	c = '{'; break;
					default: ungetcToInputFile (next);
				}
				goto enter;
			}
			case ':':
			{
				int next = getcFromInputFile ();
				if (next == '>')
					c = ']';
				else
					ungetcToInputFile (next);
				goto enter;
			}
			case '%':
			{
				int next = getcFromInputFile ();
				switch (next)
				{
					case '>':	c = '}'; break;
					case ':':	c = '#'; goto process;
					default: ungetcToInputFile (next);
				}
				goto enter;
			}

			default:
				if (c == '@' && Cpp.hasAtLiteralStrings)
				{
					int next = getcFromInputFile ();
					if (next == DOUBLE_QUOTE)
					{
						Cpp.directive.accept = FALSE;
						c = skipToEndOfString (TRUE);
						break;
					}
					else
						ungetcToInputFile (next);
				}
				else if (c == 'R' && Cpp.hasCxxRawLiteralStrings)
				{
					/* OMG!11 HACK!!11  Get the previous character.
					 *
					 * We need to know whether the previous character was an identifier or not,
					 * because "R" has to be on its own, not part of an identifier.  This allows
					 * for constructs like:
					 *
					 * 	#define FOUR "4"
					 * 	const char *p = FOUR"5";
					 *
					 * which is not a raw literal, but a preprocessor concatenation.
					 *
					 * FIXME: handle
					 *
					 * 	const char *p = R\
					 * 	"xxx(raw)xxx";
					 *
					 * which is perfectly valid (yet probably very unlikely). */
					int prev = getNthPrevCFromInputFile (1, '\0');
					int prev2 = getNthPrevCFromInputFile (2, '\0');
					int prev3 = getNthPrevCFromInputFile (3, '\0');

					if (! cppIsident (prev) ||
					    (! cppIsident (prev2) && (prev == 'L' || prev == 'u' || prev == 'U')) ||
					    (! cppIsident (prev3) && (prev2 == 'u' && prev == '8')))
					{
						int next = getcFromInputFile ();
						if (next != DOUBLE_QUOTE)
							ungetcToInputFile (next);
						else
						{
							Cpp.directive.accept = FALSE;
							c = skipToEndOfCxxRawLiteralString ();
							break;
						}
					}
				}
			enter:
				Cpp.directive.accept = FALSE;
				if (directive)
					ignore = handleDirective (c,
								  &macroCorkIndex);
				break;
		}
	} while (directive || ignore);

	DebugStatement ( debugPutc (DEBUG_CPP, c); )
	DebugStatement ( if (c == NEWLINE)
				debugPrintf (DEBUG_CPP, "%6ld: ", getInputLineNumber () + 1); )

	return c;
}
