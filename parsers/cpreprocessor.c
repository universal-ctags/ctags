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
#include "htable.h"
#include "meta-cpreprocessor.h"
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
	bool ignoreAllBranches;  /* ignoring parent conditional branch */
	bool singleBranch;       /* choose only one branch */
	bool branchChosen;       /* branch already selected */
	bool ignoring;           /* current ignore state */
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
	langType lang;
	int		ungetch, ungetch2;   /* ungotten characters, if any */
	bool resolveRequired;     /* must resolve if/else/elif/endif branch */
	bool hasAtLiteralStrings; /* supports @"c:\" strings */
	bool hasCxxRawLiteralStrings; /* supports R"xxx(...)xxx" strings */
	bool hasSingleQuoteLiteralNumbers; /* supports vera number literals:
						 'h..., 'o..., 'd..., and 'b... */
	const kindOption  *defineMacroKind;
	int macroUndefRoleIndex;
	const kindOption  *headerKind;
	int headerSystemRoleIndex;
	int headerLocalRoleIndex;

	struct sDirective {
		enum eState state;       /* current directive being processed */
		bool	accept;          /* is a directive syntactically permitted? */
		vString * name;          /* macro name */
		unsigned int nestLevel;  /* level 0 is not used */
		conditionalInfo ifdef [MaxCppNestingLevel];
	} directive;
} cppState;


typedef enum {
	CPREPRO_MACRO_KIND_UNDEF_ROLE,
} cPreProMacroRole;

static roleDesc CPREPROMacroRoles [] = {
	RoleTemplateUndef,
};


typedef enum {
	CPREPRO_HEADER_KIND_SYSTEM_ROLE,
	CPREPRO_HEADER_KIND_LOCAL_ROLE,
} cPreProHeaderRole;

static roleDesc CPREPROHeaderRoles [] = {
	RoleTemplateSystem,
	RoleTemplateLocal,
};


typedef enum {
	CPREPRO_MACRO, CPREPRO_HEADER,
} cPreProkind;

static kindOption CPreProKinds [] = {
	{ true,  'd', "macro",      "macro definitions",
	  .referenceOnly = false, ATTACH_ROLES(CPREPROMacroRoles)},
	{ true, 'h', "header",     "included header files",
	  .referenceOnly = true, ATTACH_ROLES(CPREPROHeaderRoles)},
};

/*
*   DATA DEFINITIONS
*/

static bool doesExaminCodeWithInIf0Branch;


/*  Use brace formatting to detect end of block.
 */
static bool BraceFormat = false;

static cppState Cpp = {
	LANG_IGNORE,
	'\0', '\0',  /* ungetch characters */
	false,       /* resolveRequired */
	false,       /* hasAtLiteralStrings */
	false,       /* hasCxxRawLiteralStrings */
	false,	     /* hasSingleQuoteLiteralNumbers */
	NULL,        /* defineMacroKind */
	.macroUndefRoleIndex   = ROLE_INDEX_DEFINITION,
	NULL,	     /* headerKind */
	.headerSystemRoleIndex = ROLE_INDEX_DEFINITION,
	.headerLocalRoleIndex = ROLE_INDEX_DEFINITION,
	{
		DRCTV_NONE,  /* state */
		false,       /* accept */
		NULL,        /* tag name */
		0,           /* nestLevel */
		{ {false,false,false,false} }  /* ifdef array */
	}  /* directive */
};

/*
*   FUNCTION DEFINITIONS
*/

extern bool cppIsBraceFormat (void)
{
	return BraceFormat;
}

extern unsigned int cppGetDirectiveNestLevel (void)
{
	return Cpp.directive.nestLevel;
}

extern void cppInit (const bool state, const bool hasAtLiteralStrings,
		     const bool hasCxxRawLiteralStrings,
		     const bool hasSingleQuoteLiteralNumbers,
		     const kindOption *defineMacroKind,
		     int macroUndefRoleIndex,
		     const kindOption *headerKind,
		     int headerSystemRoleIndex, int headerLocalRoleIndex)
{
	BraceFormat = state;

	if (Cpp.lang == LANG_IGNORE)
	{
		langType t;

		t = getNamedLanguage ("CPreProcessor", 0);
		initializeParser (t);
	}

	Cpp.ungetch         = '\0';
	Cpp.ungetch2        = '\0';
	Cpp.resolveRequired = false;
	Cpp.hasAtLiteralStrings = hasAtLiteralStrings;
	Cpp.hasCxxRawLiteralStrings = hasCxxRawLiteralStrings;
	Cpp.hasSingleQuoteLiteralNumbers = hasSingleQuoteLiteralNumbers;
	Cpp.defineMacroKind
		= defineMacroKind? defineMacroKind: CPreProKinds + CPREPRO_MACRO;
	Cpp.macroUndefRoleIndex
		= defineMacroKind? macroUndefRoleIndex: CPREPRO_MACRO_KIND_UNDEF_ROLE;
	Cpp.headerKind
		= headerKind? headerKind: CPreProKinds + CPREPRO_HEADER;
	Cpp.headerSystemRoleIndex
		= headerKind? headerSystemRoleIndex: CPREPRO_HEADER_KIND_SYSTEM_ROLE;
	Cpp.headerLocalRoleIndex
		= headerKind? headerLocalRoleIndex: CPREPRO_HEADER_KIND_LOCAL_ROLE;

	Cpp.directive.state     = DRCTV_NONE;
	Cpp.directive.accept    = true;
	Cpp.directive.nestLevel = 0;

	Cpp.directive.ifdef [0].ignoreAllBranches = false;
	Cpp.directive.ifdef [0].singleBranch = false;
	Cpp.directive.ifdef [0].branchChosen = false;
	Cpp.directive.ifdef [0].ignoring     = false;

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
	Cpp.resolveRequired = true;
}

extern void cppEndStatement (void)
{
	Cpp.resolveRequired = false;
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
static bool readDirective (int c, char *const name, unsigned int maxLength)
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

	return (bool) isspacetab (c);
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
	} while (c != EOF && cppIsident (c));
	ungetcToInputFile (c);
}

static void readFilename (int c, vString *const name)
{
	int c_end = (c == '<')? '>': '"';

	vStringClear (name);

	while (c = getcFromInputFile (), (c != EOF && c != c_end && c != '\n'))
		vStringPut (name, c);
}

static conditionalInfo *currentConditional (void)
{
	return &Cpp.directive.ifdef [Cpp.directive.nestLevel];
}

static bool isIgnore (void)
{
	return Cpp.directive.ifdef [Cpp.directive.nestLevel].ignoring;
}

static bool setIgnore (const bool ignore)
{
	return Cpp.directive.ifdef [Cpp.directive.nestLevel].ignoring = ignore;
}

static bool isIgnoreBranch (void)
{
	conditionalInfo *const ifdef = currentConditional ();

	/*  Force a single branch if an incomplete statement is discovered
	 *  en route. This may have allowed earlier branches containing complete
	 *  statements to be followed, but we must follow no further branches.
	 */
	if (Cpp.resolveRequired  &&  ! BraceFormat)
		ifdef->singleBranch = true;

	/*  We will ignore this branch in the following cases:
	 *
	 *  1.  We are ignoring all branches (conditional was within an ignored
	 *        branch of the parent conditional)
	 *  2.  A branch has already been chosen and either of:
	 *      a.  A statement was incomplete upon entering the conditional
	 *      b.  A statement is incomplete upon encountering a branch
	 */
	return (bool) (ifdef->ignoreAllBranches ||
					 (ifdef->branchChosen  &&  ifdef->singleBranch));
}

static void chooseBranch (void)
{
	if (! BraceFormat)
	{
		conditionalInfo *const ifdef = currentConditional ();

		ifdef->branchChosen = (bool) (ifdef->singleBranch ||
										Cpp.resolveRequired);
	}
}

/*  Pushes one nesting level for an #if directive, indicating whether or not
 *  the branch should be ignored and whether a branch has already been chosen.
 */
static bool pushConditional (const bool firstBranchChosen)
{
	const bool ignoreAllBranches = isIgnore ();  /* current ignore */
	bool ignoreBranch = false;

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
		ifdef->ignoring = (bool) (ignoreAllBranches || (
				! firstBranchChosen  &&  ! BraceFormat  &&
				(ifdef->singleBranch || !doesExaminCodeWithInIf0Branch)));
		ignoreBranch = ifdef->ignoring;
	}
	return ignoreBranch;
}

/*  Pops one nesting level for an #endif directive.
 */
static bool popConditional (void)
{
	if (Cpp.directive.nestLevel > 0)
		--Cpp.directive.nestLevel;

	return isIgnore ();
}

static int makeDefineTag (const char *const name, const char* const signature, bool undef)
{
	const bool isFileScope = (bool) (! isInputHeaderFile ());

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
		int r;

		if (Cpp.defineMacroKind == CPreProKinds + CPREPRO_MACRO)
			pushLanguage (Cpp.lang);

		if (undef)
			initRefTagEntry (&e, name, Cpp.defineMacroKind,
					 Cpp.macroUndefRoleIndex);
		else
			initTagEntry (&e, name, Cpp.defineMacroKind);
		e.isFileScope  = isFileScope;
		if (isFileScope)
			markTagExtraBit (&e, XTAG_FILE_SCOPE);
		e.truncateLine = true;
		e.extensionFields.signature = signature;

		r = makeTagEntry (&e);

		if (Cpp.defineMacroKind == CPreProKinds + CPREPRO_MACRO)
			popLanguage ();

		return r;
	}
	return CORK_NIL;
}

static bool doesCPreProRunAsStandaloneParser (void)
{
	return (Cpp.headerKind == CPreProKinds + CPREPRO_HEADER);
}

static void makeIncludeTag (const  char *const name, bool systemHeader)
{
	tagEntryInfo e;
	int role_index = systemHeader? Cpp.headerSystemRoleIndex: Cpp.headerLocalRoleIndex;

	if (role_index == ROLE_INDEX_DEFINITION)
		return;

	if (Cpp.headerKind && Cpp.headerKind->enabled
	    && isXtagEnabled (XTAG_REFERENCE_TAGS)
	    && Cpp.headerKind->roles [ role_index ].enabled)
	{
		if (doesCPreProRunAsStandaloneParser ())
			pushLanguage (Cpp.lang);

		initRefTagEntry (&e, name, Cpp.headerKind, role_index);
		e.isFileScope  = false;
		e.truncateLine = true;
		makeTagEntry (&e);

		if (doesCPreProRunAsStandaloneParser ())
			popLanguage ();
	}
}

static vString *signature;
static int directiveDefine (const int c, bool undef)
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
		directiveDefine (c, true);
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
				makeDefineTag (vStringValue (Cpp.directive.name), NULL, false);
			}
		}
	}
	Cpp.directive.state = DRCTV_NONE;
}

static bool directiveIf (const int c)
{
	DebugStatement ( const bool ignore0 = isIgnore (); )
	const bool ignore = pushConditional ((bool) (c != '0'));

	Cpp.directive.state = DRCTV_NONE;
	DebugStatement ( debugCppNest (true, Cpp.directive.nestLevel);
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

static bool directiveHash (const int c)
{
	bool ignore = false;
	char directive [MaxDirectiveName];
	DebugStatement ( const bool ignore0 = isIgnore (); )

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
		DebugStatement ( debugCppNest (false, Cpp.directive.nestLevel); )
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
static bool handleDirective (const int c, int *macroCorkIndex)
{
	bool ignore = isIgnore ();

	switch (Cpp.directive.state)
	{
		case DRCTV_NONE:    ignore = isIgnore ();        break;
		case DRCTV_DEFINE:
			*macroCorkIndex = directiveDefine (c, false);
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
static int skipToEndOfString (bool ignoreBackslash)
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
		c = skipToEndOfString (false);
	}
	else
	{
		char delim[16];
		unsigned int delimLen = 0;
		bool collectDelim = true;

		do
		{
			if (collectDelim)
			{
				if (isCxxRawLiteralDelimiterChar (c) &&
				    delimLen < (sizeof delim / sizeof *delim))
					delim[delimLen++] = c;
				else
					collectDelim = false;
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
	bool directive = false;
	bool ignore = false;
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
				ignore    = false;
				directive = false;
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
					directive = false;
				}
				Cpp.directive.accept = true;
				break;

			case DOUBLE_QUOTE:
				if (Cpp.directive.state == DRCTV_INCLUDE)
					goto enter;
				else
				{
					Cpp.directive.accept = false;
					c = skipToEndOfString (false);
				}

				break;

			case '#':
				if (Cpp.directive.accept)
				{
					directive = true;
					Cpp.directive.state  = DRCTV_HASH;
					Cpp.directive.accept = false;
				}
				break;

			case SINGLE_QUOTE:
				Cpp.directive.accept = false;
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
					Cpp.directive.accept = false;
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
						Cpp.directive.accept = false;
						c = skipToEndOfString (true);
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
							Cpp.directive.accept = false;
							c = skipToEndOfCxxRawLiteralString ();
							break;
						}
					}
				}
			enter:
				Cpp.directive.accept = false;
				if (directive)
					ignore = handleDirective (c, &macroCorkIndex);
				break;
		}
	} while (directive || ignore);

	DebugStatement ( debugPutc (DEBUG_CPP, c); )
	DebugStatement ( if (c == NEWLINE)
				debugPrintf (DEBUG_CPP, "%6ld: ", getInputLineNumber () + 1); )

	return c;
}

static void findCppTags (void)
{
	cppInit (0, false, false, false,
			 NULL, 0, NULL, 0, 0);

	findRegexTagsMainloop (cppGetc);

	cppTerminate ();
}


/*
 *  Token ignore processing
 */

static hashTable * ignoreTokenTable;

/*  Determines whether or not "name" should be ignored, per the ignore list.
 */
extern const cppIgnoredTokenInfo * cppIsIgnoreToken(const char * name)
{
	if(!ignoreTokenTable)
		return NULL;

	return (const cppIgnoredTokenInfo *)hashTableGetItem(ignoreTokenTable,(char *)name);
}

static void saveIgnoreToken(const char * ignoreToken)
{
	if(!ignoreToken)
		return;

	Assert (ignoreTokenTable);

	const char * c = ignoreToken;
	char cc = *c;

	const char * tokenBegin = c;
	const char * tokenEnd = NULL;
	const char * replacement = NULL;
	bool ignoreFollowingParenthesis = false;

	while(cc)
	{
		if(cc == '=')
		{
			if(!tokenEnd)
				tokenEnd = c;
			c++;
			if(*c)
				replacement = c;
			break;
		}

		if(cc == '+')
		{
			if(!tokenEnd)
				tokenEnd = c;
			ignoreFollowingParenthesis = true;
		}

		c++;
		cc = *c;
	}

	if(!tokenEnd)
		tokenEnd = c;

	if(tokenEnd <= tokenBegin)
		return;


	cppIgnoredTokenInfo * info = (cppIgnoredTokenInfo *)eMalloc(sizeof(cppIgnoredTokenInfo));

	info->ignoreFollowingParenthesis = ignoreFollowingParenthesis;
	info->replacement = replacement ? eStrdup(replacement) : NULL;

	hashTablePutItem(ignoreTokenTable,eStrndup(tokenBegin,tokenEnd - tokenBegin),info);

	verbose ("    ignore token: %s\n", ignoreToken);
}

static void freeIgnoredTokenInfo(cppIgnoredTokenInfo * info)
{
	if(!info)
		return;
	if(info->replacement)
		eFree(info->replacement);
	eFree(info);
}

static hashTable *makeIgnoreTokenTable (void)
{
	return hashTableNew(
		1024,
		hashCstrhash,
		hashCstreq,
		free,
		(void (*)(void *))freeIgnoredTokenInfo
		);
}

static void initializeCpp (const langType language)
{
	Cpp.lang = language;

	ignoreTokenTable = makeIgnoreTokenTable ();
}

static void CpreProInstallIgnoreToken (const langType language, const char *name, const char *arg)
{
	if (arg == NULL || arg[0] == '\0')
	{
		hashTableDelete(ignoreTokenTable);
		ignoreTokenTable = makeIgnoreTokenTable ();
		verbose ("    clearing list\n");
	}
	else
		saveIgnoreToken (arg);
}

static void CpreProSetIf0 (const langType language, const char *name, const char *arg)
{
	if (strcmp (arg, "true") == 0)
		doesExaminCodeWithInIf0Branch = true;
}

static parameterHandlerTable CpreProParameterHandlerTable [] = {
	{ .name = "if0",
	  .desc = "examine code within \"#if 0\" branch (true or [false])",
	  .handleParameter = CpreProSetIf0,
	},
	{ .name = "ignore",
	  .desc = "a token to be specially handled",
	  .handleParameter = CpreProInstallIgnoreToken,
	},
};

extern parserDefinition* CPreProParser (void)
{
	parserDefinition* const def = parserNew ("CPreProcessor");
	def->kinds      = CPreProKinds;
	def->kindCount  = ARRAY_SIZE (CPreProKinds);
	def->initialize = initializeCpp;
	def->parser     = findCppTags;

	def->parameterHandlerTable = CpreProParameterHandlerTable;
	def->parameterHandlerCount = ARRAY_SIZE(CpreProParameterHandlerTable);

	return def;
}
