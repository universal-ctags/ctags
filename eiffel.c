/*
*   $Id$
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Eiffel language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#ifdef TYPE_REFERENCE_TOOL
#include <stdio.h>
#endif
#include <string.h>
#include <limits.h>
#include <ctype.h>	/* to define tolower () */
#include <setjmp.h>

#include "debug.h"
#include "keyword.h"
#include "routines.h"
#include "vstring.h"
#ifndef TYPE_REFERENCE_TOOL
#include "entry.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#endif

/*
*   MACROS
*/
#define isident(c)		(isalnum(c) || (c) == '_')
#define isFreeOperatorChar(c)	((c) == '@' || (c) == '#' || \
				 (c) == '|' || (c) == '&')
#define isType(token,t)		(boolean) ((token)->type == (t))
#define isKeyword(token,k)	(boolean) ((token)->keyword == (k))

/*
*   DATA DECLARATIONS
*/

typedef enum eException { ExceptionNone, ExceptionEOF } exception_t;

/*  Used to specify type of keyword.
 */
typedef enum eKeywordId {
    KEYWORD_NONE,
    KEYWORD_alias, KEYWORD_all, KEYWORD_and, KEYWORD_as, KEYWORD_check,
    KEYWORD_class, KEYWORD_create, KEYWORD_creation, KEYWORD_Current,
    KEYWORD_debug, KEYWORD_deferred, KEYWORD_do, KEYWORD_else,
    KEYWORD_elseif, KEYWORD_end, KEYWORD_ensure, KEYWORD_expanded,
    KEYWORD_export, KEYWORD_external, KEYWORD_false, KEYWORD_feature,
    KEYWORD_from, KEYWORD_frozen, KEYWORD_if, KEYWORD_implies,
    KEYWORD_indexing, KEYWORD_infix, KEYWORD_inherit, KEYWORD_inspect,
    KEYWORD_invariant, KEYWORD_is, KEYWORD_like, KEYWORD_local,
    KEYWORD_loop, KEYWORD_not, KEYWORD_obsolete, KEYWORD_old, KEYWORD_once,
    KEYWORD_or, KEYWORD_prefix, KEYWORD_redefine, KEYWORD_rename,
    KEYWORD_require, KEYWORD_rescue, KEYWORD_Result, KEYWORD_retry,
    KEYWORD_select, KEYWORD_separate, KEYWORD_strip, KEYWORD_then,
    KEYWORD_true, KEYWORD_undefine, KEYWORD_unique, KEYWORD_until,
    KEYWORD_variant, KEYWORD_when, KEYWORD_xor
} keywordId;

/*  Used to determine whether keyword is valid for the token language and
 *  what its ID is.
 */
typedef struct sKeywordDesc {
    const char *name;
    keywordId id;
} keywordDesc;

typedef enum eTokenType {
    TOKEN_UNDEFINED,
    TOKEN_BANG,
    TOKEN_CHARACTER,
    TOKEN_CLOSE_BRACE,
    TOKEN_CLOSE_BRACKET,
    TOKEN_CLOSE_PAREN,
    TOKEN_COLON,
    TOKEN_COMMA,
    TOKEN_CONSTRAINT,
    TOKEN_DOT,
    TOKEN_DOLLAR,
    TOKEN_IDENTIFIER,
    TOKEN_KEYWORD,
    TOKEN_NUMERIC,
    TOKEN_OPEN_BRACE,
    TOKEN_OPEN_BRACKET,
    TOKEN_OPEN_PAREN,
    TOKEN_OPERATOR,
    TOKEN_OTHER,
    TOKEN_SEPARATOR,
    TOKEN_STRING,
    TOKEN_TILDE
} tokenType;

typedef struct sTokenInfo {
    tokenType	type;
    keywordId	keyword;
    boolean	isExported;
    vString *	string;
    vString *	className;
    vString *	featureName;
} tokenInfo;

/*
*   DATA DEFINITIONS
*/

static langType Lang_eiffel;

#ifdef TYPE_REFERENCE_TOOL

static const char *FileName;
static FILE *File;
static int PrintClass;
static int PrintReferences;
static int SelfReferences;
static int Debug;
static stringList *GenericNames;
static stringList *ReferencedTypes;

#else

typedef enum {
    EKIND_CLASS, EKIND_FEATURE, EKIND_LOCAL, EKIND_QUALIFIED_TAGS
} eiffelKind;

static kindOption EiffelKinds [] = {
    { TRUE,  'c', "class",   "classes"},
    { TRUE,  'f', "feature", "features"},
    { FALSE, 'l', "local",   "local entities"}
};

#endif

static langType Lang_eiffel;

static jmp_buf Exception;

static const keywordDesc EiffelKeywordTable [] = {
    /* keyword		keyword ID */
    { "alias",		KEYWORD_alias		},
    { "all",		KEYWORD_all		},
    { "and",		KEYWORD_and		},
    { "as",		KEYWORD_as		},
    { "check",		KEYWORD_check		},
    { "class",		KEYWORD_class		},
    { "create",		KEYWORD_create		},
    { "creation",	KEYWORD_creation	},
    { "current",	KEYWORD_Current		},
    { "debug",		KEYWORD_debug		},
    { "deferred",	KEYWORD_deferred	},
    { "do",		KEYWORD_do		},
    { "else",		KEYWORD_else		},
    { "elseif",		KEYWORD_elseif		},
    { "end",		KEYWORD_end		},
    { "ensure",		KEYWORD_ensure		},
    { "expanded",	KEYWORD_expanded	},
    { "export",		KEYWORD_export		},
    { "external",	KEYWORD_external	},
    { "false",		KEYWORD_false		},
    { "feature",	KEYWORD_feature		},
    { "from",		KEYWORD_from		},
    { "frozen",		KEYWORD_frozen		},
    { "if",		KEYWORD_if		},
    { "implies",	KEYWORD_implies		},
    { "indexing",	KEYWORD_indexing	},
    { "infix",		KEYWORD_infix		},
    { "inherit",	KEYWORD_inherit		},
    { "inspect",	KEYWORD_inspect		},
    { "invariant",	KEYWORD_invariant	},
    { "is",		KEYWORD_is		},
    { "like",		KEYWORD_like		},
    { "local",		KEYWORD_local		},
    { "loop",		KEYWORD_loop		},
    { "not",		KEYWORD_not		},
    { "obsolete",	KEYWORD_obsolete	},
    { "old",		KEYWORD_old		},
    { "once",		KEYWORD_once		},
    { "or",		KEYWORD_or		},
    { "prefix",		KEYWORD_prefix		},
    { "redefine",	KEYWORD_redefine	},
    { "rename",		KEYWORD_rename		},
    { "require",	KEYWORD_require		},
    { "rescue",		KEYWORD_rescue		},
    { "result",		KEYWORD_Result		},
    { "retry",		KEYWORD_retry		},
    { "select",		KEYWORD_select		},
    { "separate",	KEYWORD_separate	},
    { "strip",		KEYWORD_strip		},
    { "then",		KEYWORD_then		},
    { "true",		KEYWORD_true		},
    { "undefine",	KEYWORD_undefine	},
    { "unique",		KEYWORD_unique		},
    { "until",		KEYWORD_until		},
    { "variant",	KEYWORD_variant		},
    { "when",		KEYWORD_when		},
    { "xor",		KEYWORD_xor		}
};

/*
*   FUNCTION DEFINITIONS
*/

static void buildEiffelKeywordHash (void)
{
    const size_t count = sizeof (EiffelKeywordTable) /
			 sizeof (EiffelKeywordTable [0]);
    size_t i;
    for (i = 0  ;  i < count  ;  ++i)
    {
	const keywordDesc* const p = &EiffelKeywordTable [i];
	addKeyword (p->name, Lang_eiffel, (int) p->id);
    }
}

#ifdef TYPE_REFERENCE_TOOL

static void addGenericName (tokenInfo *const token)
{
    if (vStringLength (token->featureName) > 0)
	stringListAdd (GenericNames, vStringNewCopy (token->featureName));
}

static boolean isGeneric (tokenInfo *const token)
{
    return (boolean) stringListHasInsensitive (
	GenericNames, vStringValue (token->featureName));
}

static void reportType (tokenInfo *const token)
{
    if (vStringLength (token->featureName) > 0  && ! isGeneric (token)  &&
	(SelfReferences || strcasecmp (vStringValue (
	    token->featureName), vStringValue (token->className)) != 0) &&
	! stringListHasInsensitive (ReferencedTypes,
				    vStringValue (token->featureName)))
    {
	printf ("%s\n", vStringValue (token->featureName));
	stringListAdd (ReferencedTypes, vStringNewCopy (token->featureName));
	vStringClear (token->featureName);
    }
}

static int fileGetc (void)
{
    const int result = getc (File);
    if (Debug > 0  &&  result != EOF)
	putc (result, stderr);
    return result;
}

static int fileUngetc (c)
{
    return ungetc(c, File);
}

extern char *readLine (vString *const vLine, FILE *const fp)
{
    return NULL;
}

#else

/*
*   Tag generation functions
*/

static void makeEiffelClassTag (tokenInfo *const token)
{
    if (EiffelKinds [EKIND_CLASS].enabled)
    {
	const char *const name = vStringValue (token->string);
	tagEntryInfo e;

	initTagEntry (&e, name);

	e.kindName = EiffelKinds [EKIND_CLASS].name;
	e.kind     = EiffelKinds [EKIND_CLASS].letter;

	makeTagEntry (&e);
    }
    vStringCopy (token->className, token->string);
}

static void makeEiffelFeatureTag (tokenInfo *const token)
{
    if (EiffelKinds [EKIND_FEATURE].enabled  &&
	(token->isExported  ||  Option.include.fileScope))
    {
	const char *const name = vStringValue (token->string);
	tagEntryInfo e;

	initTagEntry (&e, name);

	e.isFileScope	= (boolean) (! token->isExported);
	e.kindName	= EiffelKinds [EKIND_FEATURE].name;
	e.kind		= EiffelKinds [EKIND_FEATURE].letter;
	e.extensionFields.scope [0] = EiffelKinds [EKIND_CLASS].name;
	e.extensionFields.scope [1] = vStringValue (token->className);

	makeTagEntry (&e);

	if (Option.include.qualifiedTags)
	{
	    vString* qualified = vStringNewInit (vStringValue (token->className));
	    vStringPut (qualified, '.');
	    vStringCat (qualified, token->string);
	    e.name = vStringValue (qualified);
	    makeTagEntry (&e);
	    vStringDelete (qualified);
	}
    }
    vStringCopy (token->featureName, token->string);
}

static void makeEiffelLocalTag (tokenInfo *const token)
{
    if (EiffelKinds [EKIND_LOCAL].enabled && Option.include.fileScope)
    {
	const char *const name = vStringValue (token->string);
	vString* scope = vStringNew ();
	tagEntryInfo e;

	initTagEntry (&e, name);

	e.isFileScope	= TRUE;
	e.kindName	= EiffelKinds [EKIND_LOCAL].name;
	e.kind		= EiffelKinds [EKIND_LOCAL].letter;

	vStringCopy (scope, token->className);
	vStringPut (scope, '.');
	vStringCat (scope, token->featureName);

	e.extensionFields.scope [0] = EiffelKinds [EKIND_FEATURE].name;
	e.extensionFields.scope [1] = vStringValue (scope);

	makeTagEntry (&e);
	vStringDelete (scope);
    }
}

#endif

/*
*   Parsing functions
*/

static int skipToCharacter (const int c)
{
    int d;

    do
    {
	d = fileGetc ();
    } while (d != EOF  &&  d != c);

    return d;
}

/*  If a numeric is passed in 'c', this is used as the first digit of the
 *  numeric being parsed.
 */
static vString *parseInteger (int c)
{
    static vString *string = NULL;

    if (string == NULL)
	string = vStringNew ();
    vStringClear (string);

    if (c == '\0')
	c = fileGetc ();
    if (c == '-')
    {
	vStringPut (string, c);
	c = fileGetc ();
    }
    else if (! isdigit (c))
	c = fileGetc ();
    while (c != EOF  &&  (isdigit (c)  ||  c == '_'))
    {
	vStringPut (string, c);
	c = fileGetc ();
    }
    vStringTerminate (string);
    fileUngetc (c);

    return string;
}

static vString *parseNumeric (int c)
{
    static vString *string = NULL;

    if (string == NULL)
	string = vStringNew ();
    vStringCopy (string, parseInteger (c));

    c = fileGetc ();
    if (c == '.')
    {
	vStringPut (string, c);
	vStringCat (string, parseInteger ('\0'));
	c = fileGetc ();
    }
    if (tolower (c) == 'e')
    {
	vStringPut (string, c);
	vStringCat (string, parseInteger ('\0'));
    }
    else if (!isspace (c))
	fileUngetc (c);

    vStringTerminate (string);

    return string;
}

static int parseEscapedCharacter (void)
{
    int d = '\0';
    int c = fileGetc ();

    switch (c)
    {
	case 'A':	d = '@';	break;
	case 'B':	d = '\b';	break;
	case 'C':	d = '^';	break;
	case 'D':	d = '$';	break;
	case 'F':	d = '\f';	break;
	case 'H':	d = '\\';	break;
	case 'L':	d = '~';	break;
	case 'N':	d = '\n';	break;
#ifdef QDOS
	case 'Q':	d = 0x9F;	break;
#else
	case 'Q':	d = '`';	break;
#endif
	case 'R':	d = '\r';	break;
	case 'S':	d = '#';	break;
	case 'T':	d = '\t';	break;
	case 'U':	d = '\0';	break;
	case 'V':	d = '|';	break;
	case '%':	d = '%';	break;
	case '\'':	d = '\'';	break;
	case '"':	d = '"';	break;
	case '(':	d = '[';	break;
	case ')':	d = ']';	break;
	case '<':	d = '{';	break;
	case '>':	d = '}';	break;

	case '\n': skipToCharacter ('%'); break;

	case '/':
	{
	    vString *string = parseInteger ('\0');
	    const char *value = vStringValue (string);
	    const unsigned long ascii = atol (value);

	    c = fileGetc ();
	    if (c == '/'  &&  ascii < 256)
		d = ascii;
	    break;
	}

	default: break;
    }
    return d;
}

static int parseCharacter (void)
{
    int c = fileGetc ();
    int result = c;

    if (c == '%')
	result = parseEscapedCharacter ();

    c = fileGetc ();
    if (c != '\'')
	skipToCharacter ('\n');

    return result;
}

static void parseString (vString *const string)
{
    boolean verbatim = FALSE;
    boolean align = FALSE;
    boolean end = FALSE;
    vString *verbatimCloser = NULL;
    vString *lastLine = NULL;
    int prev = '\0';
    int c;

    while (! end)
    {
	c = fileGetc ();
	if (c == EOF)
	    end = TRUE;
	else if (c == '"')
	{
	    if (! verbatim)
		end = TRUE;
	    else
		end = (boolean) (strcmp (vStringValue (lastLine),
					 vStringValue (verbatimCloser)) == 0);
	}
	else if (c == '\n')
	{
	    if (verbatim)
		vStringClear (lastLine);
	    if (prev == '[' /* ||  prev == '{' */)
	    {
		verbatim = TRUE;
		verbatimCloser = vStringNew ();
		lastLine = vStringNew ();
		if (prev == '{')
		    vStringPut (verbatimCloser, '}');
		else
		{
		    vStringPut (verbatimCloser, ']');
		    align = TRUE;
		}
		vStringNCat (verbatimCloser, string, vStringLength (string) - 1);
		vStringClear (string);
	    }
	    if (verbatim && align)
	    {
		do
		    c = fileGetc ();
		while (isspace (c));
	    }
	}
	else if (c == '%')
	    c = parseEscapedCharacter ();
	if (! end)
	{
	    vStringPut (string, c);
	    if (verbatim)
	    {
		vStringPut (lastLine, c);
		vStringTerminate (lastLine);
	    }
	    prev = c;
	}
    }
    vStringTerminate (string);
}

/*  Read a C identifier beginning with "firstChar" and places it into "name".
 */
static void parseIdentifier (vString *const string, const int firstChar)
{
    int c = firstChar;

    do
    {
	vStringPut (string, c);
	c = fileGetc ();
    } while (isident (c));

    vStringTerminate (string);
    if (!isspace (c))
	fileUngetc (c);		/* unget non-identifier character */
}

static void parseFreeOperator (vString *const string, const int firstChar)
{
    int c = firstChar;

    do
    {
	vStringPut (string, c);
	c = fileGetc ();
    } while (c > ' ');

    vStringTerminate (string);
    if (!isspace (c))
	fileUngetc (c);		/* unget non-identifier character */
}

static keywordId analyzeToken (vString *const name)
{
    static vString *keyword = NULL;
    keywordId id;

    if (keyword == NULL)
	keyword = vStringNew ();
    vStringCopyToLower (keyword, name);
    id = (keywordId) lookupKeyword (vStringValue (keyword), Lang_eiffel);

    return id;
}

static void readToken (tokenInfo *const token)
{
    int c;

    token->type    = TOKEN_UNDEFINED;
    token->keyword = KEYWORD_NONE;
    vStringClear (token->string);

getNextChar:

    do
	c = fileGetc ();
    while (c == '\t'  ||  c == ' '  ||  c == '\n');

    switch (c)
    {
	case EOF:	longjmp (Exception, (int)ExceptionEOF);	break;
	case '!':	token->type = TOKEN_BANG;		break;
	case '$':	token->type = TOKEN_DOLLAR;		break;
	case '(':	token->type = TOKEN_OPEN_PAREN;		break;
	case ')':	token->type = TOKEN_CLOSE_PAREN;	break;
	case ',':	token->type = TOKEN_COMMA;		break;
	case '.':	token->type = TOKEN_DOT;		break;
	case ';':	goto getNextChar;
	case '[':	token->type = TOKEN_OPEN_BRACKET;	break;
	case ']':	token->type = TOKEN_CLOSE_BRACKET;	break;
	case '{':	token->type = TOKEN_OPEN_BRACE;		break;
	case '}':	token->type = TOKEN_CLOSE_BRACE;	break;
	case '~':	token->type = TOKEN_TILDE;		break;


	case '+':
	case '*':
	case '^':
	case '=':	token->type = TOKEN_OPERATOR;		break;

	case '-':
	    c = fileGetc ();
	    if (c == '>')
		token->type = TOKEN_CONSTRAINT;
	    else if (c == '-')		/* is this the start of a comment? */
	    {
		skipToCharacter ('\n');
		goto getNextChar;
	    }
	    else
	    {
		if (!isspace (c))
		    fileUngetc (c);
 		token->type = TOKEN_OPERATOR;
	    }
	    break;

	case '?':
	case ':':
	    c = fileGetc ();
	    if (c == '=')
		token->type = TOKEN_OPERATOR;
	    else
	    {
		token->type = TOKEN_COLON;
		if (!isspace (c))
		    fileUngetc (c);
	    }
	    break;

	case '<':
	    c = fileGetc ();
	    if (c != '='  &&  c != '>'  &&  !isspace (c))
		fileUngetc (c);
	    token->type = TOKEN_OPERATOR;
	    break;

	case '>':
	    c = fileGetc ();
	    if (c != '='  &&  c != '>'  &&  !isspace (c))
		fileUngetc (c);
	    token->type = TOKEN_OPERATOR;
	    break;

	case '/':
	    c = fileGetc ();
	    if (c != '/'  &&  c != '='  &&  !isspace (c))
		fileUngetc (c);
	    token->type = TOKEN_OPERATOR;
	    break;

	case '\\':
	    c = fileGetc ();
	    if (c != '\\'  &&  !isspace (c))
		fileUngetc (c);
	    token->type = TOKEN_OPERATOR;
	    break;

	case '"':
	    token->type = TOKEN_STRING;
	    parseString (token->string);
	    break;

	case '\'':
	    token->type = TOKEN_CHARACTER;
	    parseCharacter ();
	    break;

	default:
	    if (isalpha (c))
	    {
		parseIdentifier (token->string, c);
		token->keyword = analyzeToken (token->string);
		if (isKeyword (token, KEYWORD_NONE))
		    token->type = TOKEN_IDENTIFIER;
		else
		    token->type = TOKEN_KEYWORD;
	    }
	    else if (isdigit (c))
	    {
		vStringCat (token->string, parseNumeric (c));
		token->type = TOKEN_NUMERIC;
	    }
	    else if (isFreeOperatorChar (c))
	    {
		parseFreeOperator (token->string, c);
		token->type = TOKEN_OPERATOR;
	    }
	    else
	    {
		token->type = TOKEN_UNDEFINED;
		Assert (! isType (token, TOKEN_UNDEFINED));
	    }
	    break;
    }
}

/*
*   Scanning functions
*/

static boolean isIdentifierMatch (const tokenInfo *const token,
				  const char *const name)
{
    return (boolean) (isType (token, TOKEN_IDENTIFIER)  &&
		     strcasecmp (vStringValue (token->string), name) == 0);
}

static void findToken (tokenInfo *const token, const tokenType type)
{
    while (! isType (token, type))
	readToken (token);
}

static void findKeyword (tokenInfo *const token, const keywordId keyword)
{
    while (! isKeyword (token, keyword))
	readToken (token);
}

static void parseGeneric (tokenInfo *const token, boolean __unused__ declaration)
{
    unsigned int depth = 0;
#ifdef TYPE_REFERENCE_TOOL
    boolean constraint = FALSE;
#endif
    Assert (isType (token, TOKEN_OPEN_BRACKET));
    do
    {
	if (isType (token, TOKEN_OPEN_BRACKET))
	    ++depth;
	else if (isType (token, TOKEN_CLOSE_BRACKET))
	    --depth;
#ifdef TYPE_REFERENCE_TOOL
	else if (declaration)
	{
	    if (depth == 1)
	    {
		if (isType (token, TOKEN_CONSTRAINT))
		    constraint = TRUE;
		else if (isKeyword (token, KEYWORD_create))
		    findKeyword (token, KEYWORD_end);
		else if (isType (token, TOKEN_IDENTIFIER))
		{
		    vStringCopy (token->featureName, token->string);
		    if (constraint)
			reportType (token);
		    else
			addGenericName (token);
		    constraint = FALSE;
		}
	    }
	    else if (isKeyword (token, KEYWORD_like))
		readToken (token);
	    else if (isType (token, TOKEN_IDENTIFIER))
	    {
		vStringCopy (token->featureName, token->string);
		reportType (token);
	    }
	}
	else
	{
	    if (isType (token, TOKEN_OPEN_BRACKET))
		++depth;
	    else if (isType (token, TOKEN_IDENTIFIER))
	    {
		vStringCopy (token->featureName, token->string);
		reportType (token);
	    }
	    else if (isKeyword (token, KEYWORD_like))
		readToken (token);
	}
#endif
	readToken (token);
    } while (depth > 0);
}

static void parseType (tokenInfo *const token)
{
    boolean bitType;
    Assert (isType (token, TOKEN_IDENTIFIER));
#ifdef TYPE_REFERENCE_TOOL
    vStringCopy (token->featureName, token->string);
    reportType (token);
#endif
    bitType = (boolean)(strcmp ("BIT", vStringValue (token->string)) == 0);
    readToken (token);
    if (bitType && isType (token, TOKEN_NUMERIC))
	readToken (token);
    else if (isType (token, TOKEN_OPEN_BRACKET))
	parseGeneric (token, FALSE);
}

static void parseEntityType (tokenInfo *const token)
{
    Assert (isType (token, TOKEN_COLON));
    readToken (token);

    if (isKeyword (token, KEYWORD_expanded))
	readToken (token);

    /*  Skip over the type name, with possible generic parameters.
     */
    if (isType (token, TOKEN_IDENTIFIER))
	parseType (token);
    else if (isKeyword (token, KEYWORD_like))
    {
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER) ||
		isKeyword (token, KEYWORD_Current))
	    readToken (token);
    }
}


static void parseLocal (tokenInfo *const token)
{
    Assert (isKeyword (token, KEYWORD_local));
    readToken (token);

    /*  Check keyword first in case local clause is empty
     */
    while (! isKeyword (token, KEYWORD_do)  &&
	   ! isKeyword (token, KEYWORD_once))
    {
#ifndef TYPE_REFERENCE_TOOL
	if (isType (token, TOKEN_IDENTIFIER))
	    makeEiffelLocalTag (token);

#endif
	readToken (token);
	if (isType (token, TOKEN_COLON))
	{
	    readToken (token);
	    if (isType (token, TOKEN_IDENTIFIER))
		parseType (token);
	}
    }
}

static void findFeatureEnd (tokenInfo *const token)
{
    readToken (token);

    switch (token->keyword)
    {
	default:
	    if (isType (token, TOKEN_OPERATOR)) /* sign of manifest constant */
		readToken (token);
	    readToken (token);		/* skip to next token after constant */
	    break;

	case KEYWORD_deferred:
	case KEYWORD_do:
	case KEYWORD_external:
	case KEYWORD_local:
	case KEYWORD_obsolete:
	case KEYWORD_once:
	case KEYWORD_require:
	{
	    int depth = 1;

	    while (depth > 0)
	    {
#ifdef TYPE_REFERENCE_TOOL
		if (isType (token, TOKEN_OPEN_BRACE))
		{
		    readToken (token);
		    if (isType (token, TOKEN_IDENTIFIER))
			parseType (token);
		    if (isType (token, TOKEN_CLOSE_BRACE))
			readToken (token);
		}
		else if (isType (token, TOKEN_BANG))
		{
		    readToken (token);
		    if (isType (token, TOKEN_IDENTIFIER))
			parseType (token);
		    if (isType (token, TOKEN_BANG))
			readToken (token);
		}
		else
#endif
		switch (token->keyword)
		{
		    case KEYWORD_check:
		    case KEYWORD_debug:
		    case KEYWORD_from:
		    case KEYWORD_if:
		    case KEYWORD_inspect:
			++depth;
			break;

		    case KEYWORD_local:
			parseLocal (token);
			break;

		    case KEYWORD_end:
			--depth;
			break;

		    default:
			break;
		}
		readToken (token);
	    }
	    break;
	}
    }
}

static boolean readFeatureName (tokenInfo *const token)
{
    boolean isFeatureName = FALSE;

    if (isKeyword (token, KEYWORD_frozen))
	readToken (token);
    if (isType (token, TOKEN_IDENTIFIER))
	isFeatureName = TRUE;
    else if (isKeyword (token, KEYWORD_infix)  ||
	    isKeyword (token, KEYWORD_prefix))
    {
	readToken (token);
	if (isType (token, TOKEN_STRING))
	    isFeatureName = TRUE;
    }
    return isFeatureName;
}

static void parseArguments (tokenInfo *const token)
{
#ifndef TYPE_REFERENCE_TOOL
    findToken (token, TOKEN_CLOSE_PAREN);
    readToken (token);
#else
    Assert (isType (token, TOKEN_OPEN_PAREN));
    readToken (token);
    do
    {
	if (! isType (token, TOKEN_COLON))
	    readToken (token);
	else
	{
	    readToken (token);
	    if (isType (token, TOKEN_IDENTIFIER))
		parseType (token);
	}
    } while (! isType (token, TOKEN_CLOSE_PAREN));
    readToken (token);
#endif
}

static boolean parseFeature (tokenInfo *const token)
{
    boolean found = FALSE;
    while (readFeatureName (token))
    {
	found = TRUE;
#ifndef TYPE_REFERENCE_TOOL
	makeEiffelFeatureTag (token);
#endif
	readToken (token);
	if (isType (token, TOKEN_COMMA))
	    readToken (token);
    }
    if (found)
    {
	if (isType (token, TOKEN_OPEN_PAREN))	/* arguments? */
	    parseArguments (token);
	if (isType (token, TOKEN_COLON))	/* a query? */
	    parseEntityType (token);
	if (isKeyword (token, KEYWORD_obsolete))
	{
	    readToken (token);
	    if (isType (token, TOKEN_STRING))
		readToken (token);
	}
	if (isKeyword (token, KEYWORD_is))
	    findFeatureEnd (token);
    }
    return found;
}

static void parseExport (tokenInfo *const token)
{
    token->isExported = TRUE;
    readToken (token);
    if (isType (token, TOKEN_OPEN_BRACE))
    {
	token->isExported = FALSE;
	while (! isType (token, TOKEN_CLOSE_BRACE))
	{
	    if (isType (token, TOKEN_IDENTIFIER))
		token->isExported |= !isIdentifierMatch (token, "NONE");
	    readToken (token);
	}
	readToken (token);
    }
}

static void parseFeatureClauses (tokenInfo *const token)
{
    Assert (isKeyword (token, KEYWORD_feature));
    do
    {
	if (isKeyword (token, KEYWORD_feature))
	    parseExport (token);
	if (! isKeyword (token, KEYWORD_invariant) &&
	    ! isKeyword (token, KEYWORD_indexing))
	{
	    if (! parseFeature (token))
		readToken (token);
	}
    } while (! isKeyword (token, KEYWORD_end) &&
	     ! isKeyword (token, KEYWORD_invariant) &&
	     ! isKeyword (token, KEYWORD_indexing));
}

static void parseRename (tokenInfo *const token)
{
    do {
	readToken (token);
	if (readFeatureName (token))
	{
	    readToken (token);
	    if (isKeyword (token, KEYWORD_as))
	    {
		readToken (token);
		if (readFeatureName (token))
		{
#ifndef TYPE_REFERENCE_TOOL
		    makeEiffelFeatureTag (token);	/* renamed feature */
#endif
		    readToken (token);
		}
	    }
	}
    } while (isType (token, TOKEN_COMMA));

    findKeyword (token, KEYWORD_end);
    readToken (token);
}


static void parseInherit (tokenInfo *const token)
{
    Assert (isKeyword (token, KEYWORD_inherit));
#ifdef TYPE_REFERENCE_TOOL
    readToken (token);
    while (isType (token, TOKEN_IDENTIFIER))
    {
	parseType (token);
	if (isType (token, TOKEN_KEYWORD))
	{
	    switch (token->keyword)	/* check for feature adaptation */
	    {
		case KEYWORD_rename:
		case KEYWORD_export:
		case KEYWORD_undefine:
		case KEYWORD_redefine:
		case KEYWORD_select:
		    findKeyword (token, KEYWORD_end);
		    readToken (token);
		default: break;
	    }
	}
    }
#else
    readToken (token);
    while (isType (token, TOKEN_IDENTIFIER))
    {
	parseType (token);
	switch (token->keyword)	    /* check for feature adaptation */
	{
	    case KEYWORD_rename:
		parseRename (token);
		if (isKeyword (token, KEYWORD_end))
		    readToken (token);
		break;

	    case KEYWORD_export:
	    case KEYWORD_undefine:
	    case KEYWORD_redefine:
	    case KEYWORD_select:
		findKeyword (token, KEYWORD_end);
		readToken (token);
		break;

	    case KEYWORD_end:
		readToken (token);
		break;

	    default: break;
	}
    }
#endif
}

static void parseClass (tokenInfo *const token)
{
    Assert (isKeyword (token, KEYWORD_class));
    readToken (token);
    if (isType (token, TOKEN_IDENTIFIER))
    {
#ifndef TYPE_REFERENCE_TOOL
	makeEiffelClassTag (token);
	readToken (token);
#else
	vStringCopy (token->className, token->string);
	if (PrintClass)
	    puts (vStringValue (token->className));
	if (! PrintReferences)
	    exit (0);
	readToken (token);
#endif
    }

    do
    {
	if (isType (token, TOKEN_OPEN_BRACKET))
	    parseGeneric (token, TRUE);
	else if (! isType (token, TOKEN_KEYWORD))
	    readToken (token);
	else switch (token->keyword)
	{
	    case KEYWORD_inherit:  parseInherit (token);        break;
	    case KEYWORD_feature:  parseFeatureClauses (token); break;
	    default:               readToken (token);           break;
	}
    } while (! isKeyword (token, KEYWORD_end));
}

static tokenInfo *newToken (void)
{
    tokenInfo *const token = xMalloc (1, tokenInfo);

    token->type		= TOKEN_UNDEFINED;
    token->keyword	= KEYWORD_NONE;
    token->isExported	= TRUE;

    token->string = vStringNew ();
    token->className = vStringNew ();
    token->featureName = vStringNew ();

    return token;
}

static void deleteToken (tokenInfo *const token)
{
    vStringDelete (token->string);
    vStringDelete (token->className);
    vStringDelete (token->featureName);

    eFree (token);
}

static void initialize (const langType language)
{
    Lang_eiffel = language;
    buildEiffelKeywordHash ();
}

static void findEiffelTags (void)
{
    tokenInfo *const token = newToken ();
    exception_t exception;

    exception = (exception_t) (setjmp (Exception));
    while (exception == ExceptionNone)
    {
	findKeyword (token, KEYWORD_class);
	parseClass (token);
    }
    deleteToken (token);
}

#ifndef TYPE_REFERENCE_TOOL

extern parserDefinition* EiffelParser (void)
{
    static const char *const extensions [] = { "e", NULL };
    parserDefinition* def = parserNew ("Eiffel");
    def->kinds      = EiffelKinds;
    def->kindCount  = KIND_COUNT (EiffelKinds);
    def->extensions = extensions;
    def->parser     = findEiffelTags;
    def->initialize = initialize;
    return def;
}

#else

static void findReferences (void)
{
    ReferencedTypes = stringListNew ();
    GenericNames = stringListNew ();
    initialize (0);

    findEiffelTags ();

    stringListDelete (GenericNames);
    GenericNames = NULL;
    stringListDelete (ReferencedTypes);
    ReferencedTypes = NULL;
}

static const char *const Usage =
    "Prints names of types referenced by an Eiffel language file.\n"
    "\n"
    "Usage: %s [-cdrs] [file_name | -]\n"
    "\n"
    "Options:\n"
    "    -c    Print class name of current file (on first line of output).\n"
    "    -d    Enable debug output.\n"
    "    -r    Print types referenced by current file (default unless -c).\n"
    "    -s    Include self-references.\n"
    "\n";

extern main (int argc, char** argv)
{
    int i;
    for (i = 1  ;  argv [i] != NULL  ;  ++i)
    {
	const char *const arg = argv [i];
	if (arg [0] == '-')
	{
	    int j;
	    if (arg [1] == '\0')
	    {
		    File = stdin;
		    FileName = "stdin";
	    }
	    else for (j = 1  ;  arg [j] != '\0'  ;  ++j) switch (arg [j])
	    {
		case 'c':  PrintClass      = 1; break;
		case 'r':  PrintReferences = 1; break;
		case 's':  SelfReferences  = 1; break;
		case 'd':  Debug           = 1; break;
		default:
		    fprintf (stderr, "%s: unknown option: %c\n", argv [0], arg [1]);
		    fprintf (stderr, Usage, argv [0]);
		    exit (1);
		    break;
	    }
	}
	else if (File != NULL)
	{
	    fprintf (stderr, Usage, argv [0]);
	    exit (1);
	}
	else
	{
	    FileName = arg;
	    File = fopen (FileName, "r");
	    if (File == NULL)
	    {
		perror (argv [0]);
		exit (1);
	    }
	}
    }
    if (! PrintClass)
	PrintReferences = 1;
    if (File == NULL)
    {
	fprintf (stderr, Usage, argv [0]);
	exit (1);
    }

    findReferences ();
}

#endif

/* vi:set tabstop=8 shiftwidth=4: */
