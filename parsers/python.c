/*
*   Copyright (c) 2000-2003, Darren Hiebert
*   Copyright (c) 2014-2016, Colomban Wendling <ban@herbesfolles.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Python language
*   files.
*/

#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "nestlevel.h"
#include "read.h"
#include "parse.h"
#include "vstring.h"
#include "keyword.h"
#include "routines.h"
#include "debug.h"
#include "xtag.h"
#include "objpool.h"
#include "ptrarray.h"

#define isIdentifierChar(c) \
	(isalnum (c) || (c) == '_' || (c) >= 0x80)
#define newToken() (objPoolGet (TokenPool))
#define deleteToken(t) (objPoolPut (TokenPool, (t)))

enum {
	KEYWORD_as,
	KEYWORD_async,
	KEYWORD_cdef,
	KEYWORD_class,
	KEYWORD_cpdef,
	KEYWORD_def,
	KEYWORD_extern,
	KEYWORD_from,
	KEYWORD_import,
	KEYWORD_inline,
	KEYWORD_lambda,
	KEYWORD_pass,
	KEYWORD_return,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum {
	ACCESS_PRIVATE,
	ACCESS_PROTECTED,
	ACCESS_PUBLIC,
	COUNT_ACCESS
} accessType;

static const char *const PythonAccesses[COUNT_ACCESS] = {
	"private",
	"protected",
	"public"
};

typedef enum {
	F_DECORATORS,
	F_NAMEREF,
	COUNT_FIELD
} pythonField;

static fieldDefinition PythonFields[COUNT_FIELD] = {
	{ .name = "decorators",
	  .description = "decorators on functions and classes",
	  .enabled = false },
	{ .name = "nameref",
	  .description = "the original name for the tag",
	  .enabled = true },
};

typedef enum {
	K_CLASS,
	K_FUNCTION,
	K_METHOD,
	K_VARIABLE,
	K_NAMESPACE,
	K_MODULE,
	K_UNKNOWN,
	K_PARAMETER,
	K_LOCAL_VARIABLE,
	COUNT_KIND
} pythonKind;

typedef enum {
	PYTHON_MODULE_IMPORTED,
	PYTHON_MODULE_NAMESPACE,
	PYTHON_MODULE_INDIRECTLY_IMPORTED,
} pythonModuleRole;

typedef enum {
	PYTHON_UNKNOWN_IMPORTED,
	PYTHON_UNKNOWN_INDIRECTLY_IMPORTED,
} pythonUnknownRole;

/* Roles related to `import'
 * ==========================
 * import X              X = (kind:module, role:imported)
 *
 * import X as Y         X = (kind:module, role:indirectlyImported),
 *                       Y = (kind:namespace, nameref:module:X)
 *                       ------------------------------------------------
 *                       Don't confuse the kind of Y with namespace role of module kind.
 *
 * from X import *       X = (kind:module,  role:namespace)
 *
 * from X import Y       X = (kind:module,  role:namespace),
 *                       Y = (kind:unknown, role:imported, scope:module:X)
 *
 * from X import Y as Z  X = (kind:module,  role:namespace),
 *                       Y = (kind:unknown, role:indirectlyImported, scope:module:X)
 *                       Z = (kind:unknown, nameref:unknown:Y) */

static roleDefinition PythonModuleRoles [] = {
	{ true, "imported",
	  "imported modules" },
	{ true, "namespace",
	  "namespace from where classes/variables/functions are imported" },
	{ true, "indirectlyImported",
	  "module imported in alternative name" },
};

static roleDefinition PythonUnknownRoles [] = {
	{ true, "imported",   "imported from the other module" },
	{ true, "indirectlyImported",
	  "classes/variables/functions/modules imported in alternative name" },
};

static kindDefinition PythonKinds[COUNT_KIND] = {
	{true, 'c', "class",    "classes"},
	{true, 'f', "function", "functions"},
	{true, 'm', "member",   "class members"},
	{true, 'v', "variable", "variables"},
	{true, 'I', "namespace", "name referring a module defined in other file"},
	{true, 'i', "module",    "modules",
	 .referenceOnly = true,  ATTACH_ROLES(PythonModuleRoles)},
	{true, 'x', "unknown",   "name referring a class/variable/function/module defined in other module",
	 .referenceOnly = false, ATTACH_ROLES(PythonUnknownRoles)},
	{false, 'z', "parameter", "function parameters" },
	{false, 'l', "local",    "local variables" },
};

static const keywordTable PythonKeywordTable[] = {
	/* keyword			keyword ID */
	{ "as",				KEYWORD_as				},
	{ "async",			KEYWORD_async			},
	{ "cdef",			KEYWORD_cdef			},
	{ "cimport",		KEYWORD_import			},
	{ "class",			KEYWORD_class			},
	{ "cpdef",			KEYWORD_cpdef			},
	{ "def",			KEYWORD_def				},
	{ "extern",			KEYWORD_extern			},
	{ "from",			KEYWORD_from			},
	{ "import",			KEYWORD_import			},
	{ "inline",			KEYWORD_inline			},
	{ "lambda",			KEYWORD_lambda			},
	{ "pass",			KEYWORD_pass			},
	{ "return",			KEYWORD_return			},
};

typedef enum eTokenType {
	/* 0..255 are the byte's value */
	TOKEN_EOF = 256,
	TOKEN_UNDEFINED,
	TOKEN_INDENT,
	TOKEN_KEYWORD,
	TOKEN_OPERATOR,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_ARROW,				/* -> */
	TOKEN_WHITESPACE,
} tokenType;

typedef struct {
	int				type;
	keywordId		keyword;
	vString *		string;
	int				indent;
	unsigned long 	lineNumber;
	MIOPos			filePosition;
} tokenInfo;

struct pythonNestingLevelUserData {
	int indentation;
};
#define PY_NL(nl) ((struct pythonNestingLevelUserData *) nestingLevelGetUserData (nl))

static langType Lang_python;
static unsigned int TokenContinuationDepth = 0;
static tokenInfo *NextToken = NULL;
static NestingLevels *PythonNestingLevels = NULL;
static objPool *TokenPool = NULL;


/* follows PEP-8, and always reports single-underscores as protected
 * See:
 * - http://www.python.org/dev/peps/pep-0008/#method-names-and-instance-variables
 * - http://www.python.org/dev/peps/pep-0008/#designing-for-inheritance
 */
static accessType accessFromIdentifier (const vString *const ident,
                                        pythonKind kind, int parentKind)
{
	const char *const p = vStringValue (ident);
	const size_t len = vStringLength (ident);

	/* inside a function/method, private */
	if (parentKind != -1 && parentKind != K_CLASS)
		return ACCESS_PRIVATE;
	/* not starting with "_", public */
	else if (len < 1 || p[0] != '_')
		return ACCESS_PUBLIC;
	/* "__...__": magic methods */
	else if (kind == K_FUNCTION && parentKind == K_CLASS &&
	         len > 3 && p[1] == '_' && p[len - 2] == '_' && p[len - 1] == '_')
		return ACCESS_PUBLIC;
	/* "__...": name mangling */
	else if (parentKind == K_CLASS && len > 1 && p[1] == '_')
		return ACCESS_PRIVATE;
	/* "_...": suggested as non-public, but easily accessible */
	else
		return ACCESS_PROTECTED;
}

static void initPythonEntry (tagEntryInfo *const e, const tokenInfo *const token,
                             const pythonKind kind)
{
	accessType access;
	int parentKind = -1;
	NestingLevel *nl;

	initTagEntry (e, vStringValue (token->string), kind);

	e->lineNumber	= token->lineNumber;
	e->filePosition	= token->filePosition;

	nl = nestingLevelsGetCurrent (PythonNestingLevels);
	if (nl)
	{
		tagEntryInfo *nlEntry = getEntryOfNestingLevel (nl);

		e->extensionFields.scopeIndex = nl->corkIndex;

		/* nlEntry can be NULL if a kind was disabled.  But what can we do
		 * here?  Even disabled kinds should count for the hierarchy I
		 * guess -- as it'd otherwise be wrong -- but with cork we're
		 * fucked up as there's nothing to look up.  Damn. */
		if (nlEntry)
		{
			parentKind = nlEntry->kindIndex;

			/* functions directly inside classes are methods, fix it up */
			if (kind == K_FUNCTION && parentKind == K_CLASS)
				e->kindIndex = K_METHOD;
		}
	}

	access = accessFromIdentifier (token->string, kind, parentKind);
	e->extensionFields.access = PythonAccesses[access];
	/* FIXME: should we really set isFileScope in addition to access? */
	if (access == ACCESS_PRIVATE)
		e->isFileScope = true;
}

static int makeClassTag (const tokenInfo *const token,
                         const vString *const inheritance,
                         const vString *const decorators)
{
	if (PythonKinds[K_CLASS].enabled)
	{
		tagEntryInfo e;

		initPythonEntry (&e, token, K_CLASS);

		e.extensionFields.inheritance = inheritance ? vStringValue (inheritance) : "";
		if (decorators && vStringLength (decorators) > 0)
		{
			attachParserField (&e, false, PythonFields[F_DECORATORS].ftype,
			                   vStringValue (decorators));
		}

		return makeTagEntry (&e);
	}

	return CORK_NIL;
}

static int makeFunctionTag (const tokenInfo *const token,
                            const vString *const arglist,
                            const vString *const decorators)
{
	if (PythonKinds[K_FUNCTION].enabled)
	{
		tagEntryInfo e;

		initPythonEntry (&e, token, K_FUNCTION);

		if (arglist)
			e.extensionFields.signature = vStringValue (arglist);
		if (decorators && vStringLength (decorators) > 0)
		{
			attachParserField (&e, false, PythonFields[F_DECORATORS].ftype,
			                   vStringValue (decorators));
		}

		return makeTagEntry (&e);
	}

	return CORK_NIL;
}

static int makeSimplePythonTag (const tokenInfo *const token, pythonKind const kind)
{
	if (PythonKinds[kind].enabled)
	{
		tagEntryInfo e;

		initPythonEntry (&e, token, kind);
		return makeTagEntry (&e);
	}

	return CORK_NIL;
}

static int makeSimplePythonRefTag (const tokenInfo *const token,
                                   const vString *const altName,
                                   pythonKind const kind,
                                   int roleIndex, xtagType xtag)
{
	if (isXtagEnabled (XTAG_REFERENCE_TAGS) &&
	    PythonKinds[kind].roles[roleIndex].enabled)
	{
		tagEntryInfo e;

		initRefTagEntry (&e, vStringValue (altName ? altName : token->string),
		                 kind, roleIndex);

		e.lineNumber	= token->lineNumber;
		e.filePosition	= token->filePosition;

		if (xtag != XTAG_UNKNOWN)
			markTagExtraBit (&e, xtag);

		return makeTagEntry (&e);
	}

	return CORK_NIL;
}

static void *newPoolToken (void *createArg CTAGS_ATTR_UNUSED)
{
	tokenInfo *token = xMalloc (1, tokenInfo);
	token->string = vStringNew ();
	return token;
}

static void deletePoolToken (void *data)
{
	tokenInfo *token = data;
	vStringDelete (token->string);
	eFree (token);
}

static void clearPoolToken (void *data)
{
	tokenInfo *token = data;

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->indent		= 0;
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	vStringClear (token->string);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const src)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	dest->indent = src->indent;
	vStringCopy(dest->string, src->string);
}

/* Skip a single or double quoted string. */
static void readString (vString *const string, const int delimiter)
{
	int escaped = 0;
	int c;

	while ((c = getcFromInputFile ()) != EOF)
	{
		if (escaped)
		{
			vStringPut (string, c);
			escaped--;
		}
		else if (c == '\\')
			escaped++;
		else if (c == delimiter || c == '\n' || c == '\r')
		{
			if (c != delimiter)
				ungetcToInputFile (c);
			break;
		}
		else
			vStringPut (string, c);
	}
}

/* Skip a single or double triple quoted string. */
static void readTripleString (vString *const string, const int delimiter)
{
	int c;
	int escaped = 0;
	int n = 0;
	while ((c = getcFromInputFile ()) != EOF)
	{
		if (c == delimiter && ! escaped)
		{
			if (++n >= 3)
				break;
		}
		else
		{
			for (; n > 0; n--)
				vStringPut (string, delimiter);
			if (c != '\\' || escaped)
				vStringPut (string, c);
			n = 0;
		}

		if (escaped)
			escaped--;
		else if (c == '\\')
			escaped++;
	}
}

static void readIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	do
	{
		vStringPut (string, (char) c);
		c = getcFromInputFile ();
	}
	while (isIdentifierChar (c));
	ungetcToInputFile (c);
}

static void ungetToken (tokenInfo *const token)
{
	Assert (NextToken == NULL);
	NextToken = newToken ();
	copyToken (NextToken, token);
}

static void readTokenFull (tokenInfo *const token, bool inclWhitespaces)
{
	int c;
	int n;

	/* if we've got a token held back, emit it */
	if (NextToken)
	{
		copyToken (token, NextToken);
		deleteToken (NextToken);
		NextToken = NULL;
		return;
	}

	token->type		= TOKEN_UNDEFINED;
	token->keyword	= KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:

	n = 0;
	do
	{
		c = getcFromInputFile ();
		n++;
	}
	while (c == ' ' || c == '\t' || c == '\f');

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	if (inclWhitespaces && n > 1 && c != '\r' && c != '\n')
	{
		ungetcToInputFile (c);
		vStringPut (token->string, ' ');
		token->type = TOKEN_WHITESPACE;
		return;
	}

	switch (c)
	{
		case EOF:
			token->type = TOKEN_EOF;
			break;

		case '\'':
		case '"':
		{
			int d = getcFromInputFile ();
			token->type = TOKEN_STRING;
			vStringPut (token->string, c);
			if (d != c)
			{
				ungetcToInputFile (d);
				readString (token->string, c);
			}
			else if ((d = getcFromInputFile ()) == c)
				readTripleString (token->string, c);
			else /* empty string */
				ungetcToInputFile (d);
			vStringPut (token->string, c);
			token->lineNumber = getInputLineNumber ();
			token->filePosition = getInputFilePosition ();
			break;
		}

		case '=':
		{
			int d = getcFromInputFile ();
			vStringPut (token->string, c);
			if (d == c)
			{
				vStringPut (token->string, d);
				token->type = TOKEN_OPERATOR;
			}
			else
			{
				ungetcToInputFile (d);
				token->type = c;
			}
			break;
		}

		case '-':
		{
			int d = getcFromInputFile ();
			if (d == '>')
			{
				vStringPut (token->string, c);
				vStringPut (token->string, d);
				token->type = TOKEN_ARROW;
				break;
			}
			ungetcToInputFile (d);
			/* fall through */
		}
		case '+':
		case '*':
		case '%':
		case '<':
		case '>':
		case '/':
		{
			int d = getcFromInputFile ();
			vStringPut (token->string, c);
			if (d != '=')
			{
				ungetcToInputFile (d);
				token->type = c;
			}
			else
			{
				vStringPut (token->string, d);
				token->type = TOKEN_OPERATOR;
			}
			break;
		}

		/* eats newline to implement line continuation  */
		case '\\':
		{
			int d = getcFromInputFile ();
			if (d == '\r')
				d = getcFromInputFile ();
			if (d != '\n')
				ungetcToInputFile (d);
			goto getNextChar;
		}

		case '#': /* comment */
		case '\r': /* newlines for indent */
		case '\n':
		{
			int indent = 0;
			do
			{
				if (c == '#')
				{
					do
						c = getcFromInputFile ();
					while (c != EOF && c != '\r' && c != '\n');
				}
				if (c == '\r')
				{
					int d = getcFromInputFile ();
					if (d != '\n')
						ungetcToInputFile (d);
				}
				indent = 0;
				while ((c = getcFromInputFile ()) == ' ' || c == '\t' || c == '\f')
				{
					if (c == '\t')
						indent += 8 - (indent % 8);
					else if (c == '\f') /* yeah, it's weird */
						indent = 0;
					else
						indent++;
				}
			} /* skip completely empty lines, so retry */
			while (c == '\r' || c == '\n' || c == '#');
			ungetcToInputFile (c);
			if (TokenContinuationDepth > 0)
			{
				if (inclWhitespaces)
				{
					vStringPut (token->string, ' ');
					token->type = TOKEN_WHITESPACE;
				}
				else
					goto getNextChar;
			}
			else
			{
				token->type = TOKEN_INDENT;
				token->indent = indent;
			}
			break;
		}

		default:
			if (! isIdentifierChar (c))
			{
				vStringPut (token->string, c);
				token->type = c;
			}
			else
			{
				/* FIXME: handle U, B, R and F string prefixes? */
				readIdentifier (token->string, c);
				token->keyword = lookupKeyword (vStringValue (token->string), Lang_python);
				if (token->keyword == KEYWORD_NONE)
					token->type = TOKEN_IDENTIFIER;
				else
					token->type = TOKEN_KEYWORD;
			}
			break;
	}

	/* handle implicit continuation lines not to emit INDENT inside brackets
	 * https://docs.python.org/3.6/reference/lexical_analysis.html#implicit-line-joining */
	if (token->type == '(' ||
	    token->type == '{' ||
	    token->type == '[')
	{
		TokenContinuationDepth ++;
	}
	else if (TokenContinuationDepth > 0 &&
	         (token->type == ')' ||
	          token->type == '}' ||
	          token->type == ']'))
	{
		TokenContinuationDepth --;
	}
}

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, false);
}

/*================================= parsing =================================*/


static void reprCat (vString *const repr, const tokenInfo *const token)
{
	if (token->type != TOKEN_INDENT &&
	    token->type != TOKEN_WHITESPACE)
	{
		vStringCat (repr, token->string);
	}
	else if (vStringLength (repr) > 0 && vStringLast (repr) != ' ')
	{
		vStringPut (repr, ' ');
	}
}

static bool skipOverPair (tokenInfo *const token, int tOpen, int tClose,
                             vString *const repr, bool reprOuterPair)
{
	if (token->type == tOpen)
	{
		int depth = 1;

		if (repr && reprOuterPair)
			reprCat (repr, token);
		do
		{
			readTokenFull (token, true);
			if (repr && (reprOuterPair || token->type != tClose || depth > 1))
			{
				reprCat (repr, token);
			}
			if (token->type == tOpen)
				depth ++;
			else if (token->type == tClose)
				depth --;
		}
		while (token->type != TOKEN_EOF && depth > 0);
	}

	return token->type == tClose;
}

static bool skipLambdaArglist (tokenInfo *const token, vString *const repr)
{
	while (token->type != TOKEN_EOF && token->type != ':' &&
	       /* avoid reading too much, just in case */
	       token->type != TOKEN_INDENT)
	{
		bool readNext = true;

		if (token->type == '(')
			readNext = skipOverPair (token, '(', ')', repr, true);
		else if (token->type == '[')
			readNext = skipOverPair (token, '[', ']', repr, true);
		else if (token->type == '{')
			readNext = skipOverPair (token, '{', '}', repr, true);
		else if (token->keyword == KEYWORD_lambda)
		{ /* handle lambdas in a default value */
			if (repr)
				reprCat (repr, token);
			readTokenFull (token, true);
			readNext = skipLambdaArglist (token, repr);
			if (token->type == ':')
				readNext = true;
			if (readNext && repr)
				reprCat (repr, token);
		}
		else if (repr)
		{
			reprCat (repr, token);
		}

		if (readNext)
			readTokenFull (token, true);
	}
	return false;
}

static void readQualifiedName (tokenInfo *const nameToken)
{
	readToken (nameToken);

	if (nameToken->type == TOKEN_IDENTIFIER ||
	    nameToken->type == '.')
	{
		vString *qualifiedName = vStringNew ();
		tokenInfo *token = newToken ();

		while (nameToken->type == TOKEN_IDENTIFIER ||
		       nameToken->type == '.')
		{
			vStringCat (qualifiedName, nameToken->string);
			copyToken (token, nameToken);

			readToken (nameToken);
		}
		/* put the last, non-matching, token back */
		ungetToken (nameToken);

		copyToken (nameToken, token);
		nameToken->type = TOKEN_IDENTIFIER;
		vStringCopy (nameToken->string, qualifiedName);

		deleteToken (token);
		vStringDelete (qualifiedName);
	}
}

static bool readCDefName (tokenInfo *const token, pythonKind *kind)
{
	readToken (token);

	if (token->keyword == KEYWORD_extern ||
	    token->keyword == KEYWORD_import)
	{
		readToken (token);
		if (token->keyword == KEYWORD_from)
			return false;
	}

	if (token->keyword == KEYWORD_class)
	{
		*kind = K_CLASS;
		readToken (token);
	}
	else
	{
		/* skip the optional type declaration -- everything on the same line
		 * until an identifier followed by "(". */
		tokenInfo *candidate = newToken ();

		while (token->type != TOKEN_EOF &&
		       token->type != TOKEN_INDENT &&
		       token->type != '=' &&
		       token->type != ',' &&
		       token->type != ':')
		{
			if (token->type == '[')
			{
				if (skipOverPair (token, '[', ']', NULL, false))
					readToken (token);
			}
			else if (token->type == '(')
			{
				if (skipOverPair (token, '(', ')', NULL, false))
					readToken (token);
			}
			else if (token->type == TOKEN_IDENTIFIER)
			{
				copyToken (candidate, token);
				readToken (token);
				if (token->type == '(')
				{ /* okay, we really found a function, use this */
					*kind = K_FUNCTION;
					ungetToken (token);
					copyToken (token, candidate);
					break;
				}
			}
			else
				readToken (token);
		}

		deleteToken (candidate);
	}

	return token->type == TOKEN_IDENTIFIER;
}

static vString *parseParamTypeAnnotation (tokenInfo *const token,
										  vString *arglist)
{
	readToken (token);
	if (token->type != ':')
	{
		ungetToken (token);
		return NULL;
	}

	reprCat (arglist, token);
	int depth = 0;
	vString *t = vStringNew ();
	while (true)
	{
		readTokenFull (token, true);
		if (token->type == TOKEN_WHITESPACE)
		{
			reprCat (arglist, token);
			continue;
		}
		else if (token->type == TOKEN_EOF)
			break;

		if (token->type == '(' ||
			token->type == '[' ||
			token->type == '{')
			depth ++;
		else if (token->type == ')' ||
				 token->type == ']' ||
				 token->type == '}')
			depth --;

		if (depth < 0
			|| (depth == 0 && (token->type == '='
							   || token->type == ',')))
		{
			ungetToken (token);
			return t;
		}
		reprCat (arglist, token);
		reprCat (t, token);
	}
	vStringDelete (t);
	return NULL;
}

static vString *parseReturnTypeAnnotation (tokenInfo *const token)
{
	readToken (token);
	if (token->type != TOKEN_ARROW)
	{
		ungetToken (token);
		return NULL;
	}

	int depth = 0;
	vString *t = vStringNew ();
	while (true)
	{
		readToken (token);
		if (token->type == TOKEN_EOF)
			break;

		if (token->type == '(' ||
			token->type == '[' ||
			token->type == '{')
			depth ++;
		else if (token->type == ')' ||
				 token->type == ']' ||
				 token->type == '}')
			depth --;
		if (depth == 0 && token->type == ':')
		{
			ungetToken (token);
			return t;
		}
		else
			reprCat (t, token);
	}
	vStringDelete (t);
	return NULL;
}

struct typedParam {
	tokenInfo *token;
	vString *type;
};

static struct typedParam *makeTypedParam (tokenInfo *token, vString *type)
{
	struct typedParam *p = xMalloc (1, struct typedParam);
	p->token = token;
	p->type = type;
	return p;
}

static struct typedParam *makeTypedParamWithCopy (const tokenInfo *token, const vString *type)
{
	tokenInfo *token_copied = newToken ();
	copyToken (token_copied, token);


	vString *type_copied = type? vStringNewCopy (type): NULL;
	return makeTypedParam (token_copied, type_copied);
}

static void deleteTypedParam (struct typedParam *p)
{
	deleteToken (p->token);
	vStringDelete (p->type);	/* NULL is acceptable. */
	eFree (p);
}

static void parseArglist (tokenInfo *const token, const int kind,
						  vString *const arglist, ptrArray *const parameters)
{
	int prevTokenType = token->type;
	int depth = 1;

	if (kind != K_CLASS)
		reprCat (arglist, token);

	do
	{
		if (token->type != TOKEN_WHITESPACE &&
			/* for easy `*args` and `**kwargs` support, we also ignore
			 * `*`, which anyway can't otherwise screw us up */
			token->type != '*')
		{
			prevTokenType = token->type;
		}

		readTokenFull (token, true);
		if (kind != K_CLASS || token->type != ')' || depth > 1)
			reprCat (arglist, token);

		if (token->type == '(' ||
			token->type == '[' ||
			token->type == '{')
			depth ++;
		else if (token->type == ')' ||
				 token->type == ']' ||
				 token->type == '}')
			depth --;
		else if (kind != K_CLASS && depth == 1 &&
				 token->type == TOKEN_IDENTIFIER &&
				 (prevTokenType == '(' || prevTokenType == ',') &&
				 PythonKinds[K_PARAMETER].enabled)
		{
			tokenInfo *parameterName;
			vString *parameterType;
			struct typedParam *parameter;

			parameterName = newToken ();
			copyToken (parameterName, token);
			parameterType = parseParamTypeAnnotation (token, arglist);

			parameter = makeTypedParam (parameterName, parameterType);
			ptrArrayAdd (parameters, parameter);
		}
	}
	while (token->type != TOKEN_EOF && depth > 0);
}

static void parseCArglist (tokenInfo *const token, const int kind,
						  vString *const arglist, ptrArray *const parameters)
{
	int depth = 1;
	tokenInfo *pname = newToken ();
	vString *ptype = vStringNew ();
	vStringCat (arglist, token->string);	/* '(' */

	while (true)
	{
		readToken (token);
		if (token->type == TOKEN_EOF)
		{
			/* Unexpected input. */
			vStringClear (arglist);
			ptrArrayClear (parameters);
			break;
		}

		if (depth == 1 && (token->type == ',' || token->type == ')'))
		{
			if (pname->type == TOKEN_IDENTIFIER)
			{
				struct typedParam *p;

				/*
				 * Clean up the type string.
				 * The type string includes the parameter name at the end.
				 * 1. Trim the parameter name at the end.
				 * 2. Then, trim the white space at the end of the type string.
				 * 3. If the type string is not empty,
				 *    3.a append (the type stirng + ' ' + the parameter name) to arglist.
				 *    3.b else just append the parameter name to arglist.
				 *
				 * FIXME:
				 * This doesn't work well with an array and a function pointer.
				 *
				 *   f(..., int seq [dim], ...)
				 *      in this case, dim is extacted as a parameter.
				 *
				 *   f(..., int (*fn)(int), ...)
				 *      in this case , int is extacted as a parameter.
				 */
				Assert (vStringLength (ptype) >= vStringLength (pname->string));
				size_t ptype_len = vStringLength (ptype) - vStringLength (pname->string);
				vStringTruncate (ptype, ptype_len);

				if (vStringLength (ptype) > 0)
				{
					vStringStripTrailing (ptype);
					if (vStringLength (ptype) > 0)
					{
						vStringCat (arglist, ptype);
						vStringPut (arglist, ' ');
					}
				}
				vStringCat (arglist, pname->string);

				p = makeTypedParamWithCopy (pname, vStringIsEmpty(ptype)? NULL: ptype);
				ptrArrayAdd (parameters, p);
			}
			if (token->type == ')')
			{
				vStringPut (arglist, ')');
				break;
			}
			vStringCatS (arglist, ", ");
			vStringClear (ptype);
			pname->type = TOKEN_UNDEFINED;
			continue;
		}

		if (token->type == '(' ||
			token->type == '[' ||
			token->type == '{')
		{
			vStringPut (ptype, token->type);
			depth ++;
			continue;
		}

		if (token->type == ')' ||
			token->type == ']' ||
			token->type == '}')
		{
			vStringPut (ptype, token->type);
			depth --;
			continue;
		}

		if (token->type == TOKEN_IDENTIFIER || token->type == TOKEN_KEYWORD)
		{
			if (vStringLength (ptype) > 0
				&& (isalnum ((unsigned char)vStringLast (ptype))
					|| vStringLast (ptype) == ','))
				vStringPut (ptype, ' ');
			vStringCat (ptype, token->string);

			if (!isdigit ((unsigned char)vStringLast (token->string)))
				copyToken (pname, token);
			continue;
		}

		vStringCat (ptype, token->string);
	}

	vStringDelete (ptype);
	deleteToken (pname);
}

static bool parseClassOrDef (tokenInfo *const token,
                                const vString *const decorators,
                                pythonKind kind, bool isCDef)
{
	vString *arglist = NULL;
	tokenInfo *name = NULL;
	ptrArray *parameters = NULL;
	NestingLevel *lv;
	int corkIndex;

	if (isCDef)
	{
		if (! readCDefName (token, &kind))
			return false;
	}
	else
	{
		readToken (token);
		if (token->type != TOKEN_IDENTIFIER)
			return false;
	}

	name = newToken ();
	copyToken (name, token);

	readToken (token);
	/* collect parameters or inheritance */
	if (token->type == '(')
	{
		arglist = vStringNew ();
		parameters = ptrArrayNew ((ptrArrayDeleteFunc)deleteTypedParam);

		if (isCDef && kind != K_CLASS)
			parseCArglist (token, kind, arglist, parameters);
		else
			parseArglist (token, kind, arglist, parameters);
	}

	if (kind == K_CLASS)
		corkIndex = makeClassTag (name, arglist, decorators);
	else
		corkIndex = makeFunctionTag (name, arglist, decorators);

	lv = nestingLevelsPush (PythonNestingLevels, corkIndex);
	PY_NL (lv)->indentation = token->indent;

	deleteToken (name);
	vStringDelete (arglist);

	if (parameters && !ptrArrayIsEmpty (parameters))
	{
		unsigned int i;

		for (i = 0; i < ptrArrayCount (parameters); i++)
		{
			struct typedParam *parameter = ptrArrayItem (parameters, i);
			int paramCorkIndex = makeSimplePythonTag (parameter->token, K_PARAMETER);
			tagEntryInfo *e = getEntryInCorkQueue (paramCorkIndex);
			if (e && parameter->type)
			{
				e->extensionFields.typeRef [0] = eStrdup ("typename");
				e->extensionFields.typeRef [1] = vStringDeleteUnwrap (parameter->type);
				parameter->type = NULL;
			}
		}
	}
	ptrArrayDelete (parameters); /* NULL is acceptable. */

	tagEntryInfo *e;
	vString *t;
	if (kind != K_CLASS
		&& (e = getEntryInCorkQueue (corkIndex))
		&& (t = parseReturnTypeAnnotation (token)))
	{
		e->extensionFields.typeRef [0] = eStrdup ("typename");
		e->extensionFields.typeRef [1] = vStringDeleteUnwrap (t);
	}

	return true;
}

static bool parseImport (tokenInfo *const token)
{
	tokenInfo *fromModule = NULL;

	if (token->keyword == KEYWORD_from)
	{
		readQualifiedName (token);
		if (token->type == TOKEN_IDENTIFIER)
		{
			fromModule = newToken ();
			copyToken (fromModule, token);
			readToken (token);
		}
	}

	if (token->keyword == KEYWORD_import)
	{
		bool parenthesized = false;
		int moduleIndex;

		if (fromModule)
		{
			/* from X import ...
			 * --------------------
			 * X = (kind:module, role:namespace) */
			moduleIndex = makeSimplePythonRefTag (fromModule, NULL, K_MODULE,
												  PYTHON_MODULE_NAMESPACE,
												  XTAG_UNKNOWN);
		}

		do
		{
			readQualifiedName (token);

			/* support for `from x import (...)` */
			if (fromModule && ! parenthesized && token->type == '(')
			{
				parenthesized = true;
				readQualifiedName (token);
			}

			if (token->type == TOKEN_IDENTIFIER)
			{
				tokenInfo *name = newToken ();

				copyToken (name, token);
				readToken (token);
				/* if there is an "as", use it as the name */
				if (token->keyword == KEYWORD_as)
				{
					readToken (token);
					if (token->type == TOKEN_IDENTIFIER)
					{
						if (fromModule)
						{
							/* from x import Y as Z
							 * ----------------------------
							 * x = (kind:module,  role:namespace),
							 * Y = (kind:unknown, role:indirectlyImported, scope:module:X),
							 * Z = (kind:unknown, nameref:unknown:Y) */
							int index;

							/* Y */
							index = makeSimplePythonRefTag (name, NULL, K_UNKNOWN,
															PYTHON_UNKNOWN_INDIRECTLY_IMPORTED,
															XTAG_UNKNOWN);
							/* fill the scope field for Y */
							tagEntryInfo *e = getEntryInCorkQueue (index);
							if (e)
								e->extensionFields.scopeIndex = moduleIndex;

							/* Z */
							index = makeSimplePythonTag (token, K_UNKNOWN);
							/* fill the nameref filed for Y */
							if (PythonFields[F_NAMEREF].enabled)
							{
								vString *nameref = vStringNewInit (PythonKinds [K_UNKNOWN].name);
								vStringPut (nameref, ':');
								vStringCat (nameref, name->string);
								attachParserFieldToCorkEntry (index, PythonFields[F_NAMEREF].ftype,
															  vStringValue (nameref));
								vStringDelete (nameref);
							}
						}
						else
						{
							/* import x as Y
							 * ----------------------------
							 * x = (kind:module, role:indirectlyImported)
							 * Y = (kind:namespace, nameref:module:x)*/
							/* x */
							makeSimplePythonRefTag (name, NULL, K_MODULE,
							                        PYTHON_MODULE_INDIRECTLY_IMPORTED,
							                        XTAG_UNKNOWN);
							/* Y */
							int index = makeSimplePythonTag (token, K_NAMESPACE);
							/* fill the nameref filed for Y */
							if (PythonFields[F_NAMEREF].enabled)
							{
								vString *nameref = vStringNewInit (PythonKinds [K_MODULE].name);
								vStringPut (nameref, ':');
								vStringCat (nameref, name->string);
								attachParserFieldToCorkEntry (index, PythonFields[F_NAMEREF].ftype,
															  vStringValue (nameref));
								vStringDelete (nameref);
							}
						}

						copyToken (name, token);
						readToken (token);
					}
				}
				else
				{
					if (fromModule)
					{
						/* from x import Y
						   --------------
						   x = (kind:module,  role:namespace),
						   Y = (kind:unknown, role:imported, scope:module:x) */
						/* Y */
						int index = makeSimplePythonRefTag (name, NULL, K_UNKNOWN,
															PYTHON_UNKNOWN_IMPORTED,
															XTAG_UNKNOWN);
						/* fill the scope field for Y */
						tagEntryInfo *e = getEntryInCorkQueue (index);
						if (e)
							e->extensionFields.scopeIndex = moduleIndex;
					}
					else
					{
						/* import X
						   --------------
						   X = (kind:module, role:imported) */
						makeSimplePythonRefTag (name, NULL, K_MODULE,
						                        PYTHON_MODULE_IMPORTED,
						                        XTAG_UNKNOWN);
					}
				}

				deleteToken (name);
			}
		}
		while (token->type == ',');

		if (parenthesized && token->type == ')')
			readToken (token);
	}

	if (fromModule)
		deleteToken (fromModule);

	return false;
}

/* this only handles the most common cases, but an annotation can be any
 * expression in theory.
 * this function assumes there must be an annotation, and doesn't do any check
 * on the token on which it is called: the caller should do that part. */
static bool skipVariableTypeAnnotation (tokenInfo *const token, vString *const repr)
{
	bool readNext = true;

	readToken (token);
	switch (token->type)
	{
		case '[': readNext = skipOverPair (token, '[', ']', repr, true); break;
		case '(': readNext = skipOverPair (token, '(', ')', repr, true); break;
		case '{': readNext = skipOverPair (token, '{', '}', repr, true); break;
		default: reprCat (repr, token);
	}
	if (readNext)
		readToken (token);
	/* skip subscripts and calls */
	while (token->type == '[' || token->type == '(' || token->type == '.' || token->type == '|')
	{
		switch (token->type)
		{
			case '[': readNext = skipOverPair (token, '[', ']', repr, true); break;
			case '(': readNext = skipOverPair (token, '(', ')', repr, true); break;
			case '|':
				reprCat (repr, token);
				skipVariableTypeAnnotation (token, repr);
				readNext = false;
				break;
			case '.':
				reprCat (repr, token);
				readToken (token);
				readNext = token->type == TOKEN_IDENTIFIER;
				if (readNext)
					reprCat (repr, token);
				break;
			default:  readNext = false; break;
		}
		if (readNext)
			readToken (token);
	}

	return false;
}

static bool parseVariable (tokenInfo *const token, const pythonKind kind)
{
	/* In order to support proper tag type for lambdas in multiple
	 * assignations, we first collect all the names, and then try and map
	 * an assignation to it */
	tokenInfo *nameTokens[8] = { NULL };
	vString   *nameTypes [ARRAY_SIZE (nameTokens)] = { NULL };
	unsigned int nameCount = 0;
	vString *type = vStringNew();

	/* first, collect variable name tokens */
	while (token->type == TOKEN_IDENTIFIER &&
	       nameCount < ARRAY_SIZE (nameTokens))
	{
		unsigned int i;
		tokenInfo *name = newToken ();
		copyToken (name, token);

		readToken (token);
		if (token->type == '.')
		{
			/* FIXME: what to do with dotted names?  We currently ignore them
			 *        as we need to do something not to break the whole
			 *        declaration, but the expected behavior is questionable */
			deleteToken (name);
			name = NULL;

			do
			{
				readToken (token);
			}
			while (token->type == TOKEN_IDENTIFIER ||
			       token->type == '.');
		}

		i = nameCount++;
		nameTokens[i] = name;

		/* (parse and) skip annotations.  we need not to be too permissive because we
		 * aren't yet sure we're actually parsing a variable. */
		if (token->type == ':' && skipVariableTypeAnnotation (token, type))
			readToken (token);

		if (vStringLength (type) > 0)
		{
			nameTypes[i] = type;
			type = vStringNew ();
		}

		if (token->type == ',')
			readToken (token);
		else
			break;
	}
	vStringDelete (type);

	/* then, if it's a proper assignation, try and map assignations so that
	 * we catch lambdas and alike */
	if (token->type == '=')
	{
		unsigned int i = 0;

		do
		{
			const tokenInfo *const nameToken = nameTokens[i];
			vString **type = &(nameTypes[i++]);

			readToken (token);

			if (! nameToken)
				/* nothing */;
			else if (token->keyword != KEYWORD_lambda)
			{
				int index = makeSimplePythonTag (nameToken, kind);
				tagEntryInfo *e = getEntryInCorkQueue (index);
				if (e && *type)
				{
					e->extensionFields.typeRef [0] = eStrdup ("typename");
					e->extensionFields.typeRef [1] = vStringDeleteUnwrap (*type);
					*type = NULL;
				}
			}
			else
			{
				tokenInfo *anon  = NULL;
				vString *arglist = vStringNew ();
				if (*type)
				{
					anon = newToken ();
					copyToken (anon, token);
				}
				readToken (token);
				vStringPut (arglist, '(');
				skipLambdaArglist (token, arglist);
				vStringPut (arglist, ')');
				if (*type)
				{
					/* How to handle lambda assigned to a variable
					 * --------------------------------------------
					 *
					 * input.py:
					 *
					 * 	  id = lambda var: var
					 * 	  id_t: Callable[[int], int] = lambda var: var
					 *
					 * `id' is tagged as a function kind object like:
					 *
					 *    id	input.py	/^id = lambda var: var$/;"	function
					 *
					 * For `id_t' we cannot do the same as `id'.
					 *
					 * We should not store `Callable[[int], int]' to typeref
					 * field of the tag of `id_t' if the tag has "function" as
					 * its kind because users expect the typeref field of a
					 * function kind represents a type for the value returned
					 * from the function (return type).
					 *
					 * the unexpected tag:
					 *
					 *    id_t	input.py	/^id_t: Callable[[int], int] = lambda var: var$/;"	function \
					 *                          typeref:typename:Callable[[int], int]
					 *
					 * If we make a tag for `id_t' as a function, we should
					 * attach `typeref:typename:int' and `signature:(int)'. To
					 * achieve this, we have to make ctags analyze
					 * `Callable[[int], int]'.  However, we want to avoid the
					 * level of analyzing.
					 *
					 * For recording `Callable[[int], int]', a valuable
					 * information in the input, we use indirection.
					 *
					 *    id_t	input.py	/^id_t: Callable[[int], int] = lambda var: var$/;"	variable \
					 *                          typeref:typename:Callable[[int], int]	nameref:function:anonFuncNNN
					 *    anonFuncNNN	input.py	/^id_t: Callable[[int], int] = lambda var: var$/;"	function \
					 *                          extras:anonymous
					 */
					int vindex = makeSimplePythonTag (nameToken, kind);
					vStringClear (anon->string);
					anonGenerate (anon->string, "anonFunc", K_FUNCTION);
					int findex = makeFunctionTag (anon, arglist, NULL);
					tagEntryInfo *fe = getEntryInCorkQueue (findex);
					if (fe)
						markTagExtraBit (fe, XTAG_ANONYMOUS);

					tagEntryInfo *ve = getEntryInCorkQueue (vindex);
					if (ve)
					{
						ve->extensionFields.typeRef [0] = eStrdup ("typename");
						ve->extensionFields.typeRef [1] = vStringDeleteUnwrap (*type);
						*type = NULL;
						vString *nameref = vStringNewInit (PythonKinds [K_FUNCTION].name);
						vStringPut (nameref, ':');
						vStringCat (nameref, anon->string);
						attachParserField (ve, true, PythonFields[F_NAMEREF].ftype,
										   vStringValue (nameref));
						vStringDelete (nameref);
					}
					if (anon)
						deleteToken (anon);
				}
				else
					makeFunctionTag (nameToken, arglist, NULL);
				vStringDelete (arglist);
			}

			/* skip until next initializer */
			while ((TokenContinuationDepth > 0 || token->type != ',') &&
			       token->type != TOKEN_EOF &&
			       token->type != ';' &&
			       token->type != TOKEN_INDENT)
			{
				readToken (token);
			}
		}
		while (token->type == ',' && i < nameCount);

		/* if we got leftover to initialize, just make variables out of them.
		 * This handles cases like `a, b, c = (c, d, e)` -- or worse */
		for (; i < nameCount; i++)
		{
			if (nameTokens[i])
				makeSimplePythonTag (nameTokens[i], kind);
		}
	}

	while (nameCount > 0)
	{
		if (nameTokens[--nameCount])
			deleteToken (nameTokens[nameCount]);
		vStringDelete (nameTypes[nameCount]); /* NULL is acceptable. */
	}

	return false;
}

/* pops any level >= to indent */
static void setIndent (tokenInfo *const token)
{
	NestingLevel *lv = nestingLevelsGetCurrent (PythonNestingLevels);

	while (lv && PY_NL (lv)->indentation >= token->indent)
	{
		tagEntryInfo *e = getEntryInCorkQueue (lv->corkIndex);
		if (e)
			e->extensionFields.endLine = token->lineNumber;

		nestingLevelsPop (PythonNestingLevels);
		lv = nestingLevelsGetCurrent (PythonNestingLevels);
	}
}

static void findPythonTags (void)
{
	tokenInfo *const token = newToken ();
	vString *decorators = vStringNew ();
	bool atStatementStart = true;

	TokenContinuationDepth = 0;
	NextToken = NULL;
	PythonNestingLevels = nestingLevelsNew (sizeof (struct pythonNestingLevelUserData));

	readToken (token);
	while (token->type != TOKEN_EOF)
	{
		tokenType iterationTokenType = token->type;
		bool readNext = true;

		/* skip async keyword that confuses decorator parsing before a def */
		if (token->keyword == KEYWORD_async)
			readToken (token);

		if (token->type == TOKEN_INDENT)
			setIndent (token);
		else if (token->keyword == KEYWORD_class ||
		         token->keyword == KEYWORD_def)
		{
			pythonKind kind = token->keyword == KEYWORD_class ? K_CLASS : K_FUNCTION;

			readNext = parseClassOrDef (token, decorators, kind, false);
		}
		else if (token->keyword == KEYWORD_cdef ||
		         token->keyword == KEYWORD_cpdef)
		{
			readNext = parseClassOrDef (token, decorators, K_FUNCTION, true);
		}
		else if (token->keyword == KEYWORD_from ||
		         token->keyword == KEYWORD_import)
		{
			readNext = parseImport (token);
		}
		else if (token->type == '(')
		{ /* skip parentheses to avoid finding stuff inside them */
			readNext = skipOverPair (token, '(', ')', NULL, false);
		}
		else if (token->type == TOKEN_IDENTIFIER && atStatementStart)
		{
			NestingLevel *lv = nestingLevelsGetCurrent (PythonNestingLevels);
			tagEntryInfo *lvEntry = getEntryOfNestingLevel (lv);
			pythonKind kind = K_VARIABLE;

			if (lvEntry && lvEntry->kindIndex != K_CLASS)
				kind = K_LOCAL_VARIABLE;

			readNext = parseVariable (token, kind);
		}
		else if (token->type == '@' && atStatementStart &&
		         PythonFields[F_DECORATORS].enabled)
		{
			/* collect decorators */
			readQualifiedName (token);
			if (token->type != TOKEN_IDENTIFIER)
				readNext = false;
			else
			{
				if (vStringLength (decorators) > 0)
					vStringPut (decorators, ',');
				vStringCat (decorators, token->string);
				readToken (token);
				readNext = skipOverPair (token, '(', ')', decorators, true);
			}
		}

		/* clear collected decorators for any non-decorator tokens non-indent
		 * token.  decorator collection takes care of skipping the possible
		 * argument list, so we should never hit here parsing a decorator */
		if (iterationTokenType != TOKEN_INDENT &&
		    iterationTokenType != '@' &&
		    PythonFields[F_DECORATORS].enabled)
		{
			vStringClear (decorators);
		}

		atStatementStart = (token->type == TOKEN_INDENT || token->type == ';');

		if (readNext)
			readToken (token);
	}

	nestingLevelsFree (PythonNestingLevels);
	vStringDelete (decorators);
	deleteToken (token);
	Assert (NextToken == NULL);
}

static void initialize (const langType language)
{
	Lang_python = language;

	TokenPool = objPoolNew (16, newPoolToken, deletePoolToken, clearPoolToken, NULL);
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (!initialized)
		return;

	objPoolDelete (TokenPool);
}

extern parserDefinition* PythonParser (void)
{
	static const char *const extensions[] = { "py", "pyx", "pxd", "pxi", "scons",
											  "wsgi", NULL };
	static const char *const aliases[] = { "python[23]*", "scons", NULL };
	parserDefinition *def = parserNew ("Python");
	def->kindTable = PythonKinds;
	def->kindCount = ARRAY_SIZE (PythonKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser = findPythonTags;
	def->initialize = initialize;
	def->finalize = finalize;
	def->keywordTable = PythonKeywordTable;
	def->keywordCount = ARRAY_SIZE (PythonKeywordTable);
	def->fieldTable = PythonFields;
	def->fieldCount = ARRAY_SIZE (PythonFields);
	def->useCork = CORK_QUEUE;
	def->requestAutomaticFQTag = true;
	return def;
}
