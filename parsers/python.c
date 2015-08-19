/*
*   Copyright (c) 2000-2003, Darren Hiebert
*   Copyright (c) 2014-2015, Colomban Wendling <ban@herbesfolles.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for generating tags for Python language
*   files.
*/

#include "general.h"  /* must always come first */

#include <string.h>

#include "entry.h"
#include "nestlevel.h"
#include "read.h"
#include "main.h"
#include "parse.h"
#include "vstring.h"
#include "keyword.h"
#include "routines.h"
#include "debug.h"


typedef enum {
	KEYWORD_NONE = -1,
	KEYWORD_as,
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
} keywordId;

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
	K_CLASS,
	K_FUNCTION,
	K_METHOD,
	K_VARIABLE,
	K_IMPORT,
	COUNT_KIND
} pythonKind;

static kindOption PythonKinds[COUNT_KIND] = {
	{TRUE, 'c', "class",    "classes"},
	{TRUE, 'f', "function", "functions"},
	{TRUE, 'm', "member",   "class members"},
	{TRUE, 'v', "variable", "variables"},
	{TRUE, 'i', "namespace", "imports"}
};

typedef struct {
	const char *name;
	keywordId id;
} keywordDesc;

static const keywordDesc PythonKeywordTable[] = {
	/* keyword			keyword ID */
	{ "as",				KEYWORD_as				},
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
	TOKEN_WHITESPACE,
} tokenType;

typedef struct {
	int				type;
	keywordId		keyword;
	vString *		string;
	int				indent;
	unsigned long 	lineNumber;
	fpos_t			filePosition;
} tokenInfo;

static langType Lang_python;
static tokenInfo *NextToken = NULL;
static NestingLevels *PythonNestingLevels = NULL;


static void buildPythonKeywordHash (void)
{
	const size_t count = sizeof (PythonKeywordTable) / sizeof (PythonKeywordTable[0]);
	size_t i;
	for (i = 0; i < count ; i++)
	{
		const keywordDesc* const p = &PythonKeywordTable[i];
		addKeyword (p->name, Lang_python, (int) p->id);
	}
}

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
	static vString *fullScope = NULL;
	int parentKind = -1;

	if (fullScope == NULL)
		fullScope = vStringNew ();
	else
		vStringClear (fullScope);

	initTagEntry (e, vStringValue (token->string));

	e->lineNumber	= token->lineNumber;
	e->filePosition	= token->filePosition;
	e->kindName		= PythonKinds[kind].name;
	e->kind			= (char) PythonKinds[kind].letter;

	if (PythonNestingLevels->n > 0)
	{
		int i;

		for (i = 0; i < PythonNestingLevels->n; i++)
		{
			parentKind = PythonNestingLevels->levels[i].type;
			if (vStringLength (fullScope) > 0)
				vStringPut (fullScope, '.');
			vStringCat (fullScope, PythonNestingLevels->levels[i].name);
		}
	}

	access = accessFromIdentifier (token->string, kind, parentKind);
	e->extensionFields.access = PythonAccesses[access];
	/* FIXME: should we really set isFileScope in addition to access? */
	if (access == ACCESS_PRIVATE)
		e->isFileScope = TRUE;

	if (vStringLength (fullScope) > 0)
	{
		Assert (parentKind >= 0);

		vStringTerminate (fullScope);
		e->extensionFields.scope[0] = PythonKinds[parentKind].name;
		e->extensionFields.scope[1] = vStringValue (fullScope);

		if (kind == K_FUNCTION && parentKind == K_CLASS)
		{
			e->kindName	= PythonKinds[K_METHOD].name;
			e->kind		= (char) PythonKinds[K_METHOD].letter;
		}
	}
}

static void makeClassTag (const tokenInfo *const token,
                          vString *const inheritance)
{
	if (PythonKinds[K_CLASS].enabled)
	{
		tagEntryInfo e;

		initPythonEntry (&e, token, K_CLASS);

		if (inheritance && vStringLength (inheritance) > 0)
			e.extensionFields.inheritance = vStringValue (inheritance);

		makeTagEntry (&e);
	}
}

static void makeFunctionTag (const tokenInfo *const token,
                             const vString *const arglist)
{
	if (PythonKinds[K_FUNCTION].enabled)
	{
		tagEntryInfo e;

		initPythonEntry (&e, token, K_FUNCTION);

		if (arglist)
			e.extensionFields.signature = vStringValue (arglist);

		makeTagEntry (&e);
	}
}

static void makeSimplePythonTag (const tokenInfo *const token, pythonKind const kind)
{
	if (PythonKinds[kind].enabled)
	{
		tagEntryInfo e;

		initPythonEntry (&e, token, kind);
		makeTagEntry (&e);
	}
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->string		= vStringNew ();
	token->indent		= 0;
	token->lineNumber   = getSourceLineNumber ();
	token->filePosition = getInputFilePosition ();

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	eFree (token);
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

static boolean isIdentifierChar (const int c)
{
	return (isalnum (c) || c == '_' || c >= 0x80);
}

/* Skip a single or double quoted string. */
static void readString (vString *const string, const int delimiter)
{
	int escaped = 0;
	int c;

	while ((c = fileGetc ()) != EOF)
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
				fileUngetc (c);
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
	while ((c = fileGetc ()) != EOF)
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
		c = fileGetc ();
	}
	while (isIdentifierChar (c));
	fileUngetc (c);
	vStringTerminate (string);
}

static void ungetToken (tokenInfo *const token)
{
	Assert (NextToken == NULL);
	NextToken = newToken ();
	copyToken (NextToken, token);
}

static void readTokenFull (tokenInfo *const token, boolean inclWhitespaces)
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
		c = fileGetc ();
		n++;
	}
	while (c == ' ' || c == '\t' || c == '\f');

	token->lineNumber   = getSourceLineNumber ();
	token->filePosition = getInputFilePosition ();

	if (inclWhitespaces && n > 1 && c != '\r' && c != '\n')
	{
		fileUngetc (c);
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
			int d = fileGetc ();
			token->type = TOKEN_STRING;
			vStringPut (token->string, c);
			if (d != c)
			{
				fileUngetc (d);
				readString (token->string, c);
			}
			else if ((d = fileGetc ()) == c)
				readTripleString (token->string, c);
			else /* empty string */
				fileUngetc (d);
			vStringPut (token->string, c);
			token->lineNumber = getSourceLineNumber ();
			token->filePosition = getInputFilePosition ();
			break;
		}

		case '=':
		{
			int d = fileGetc ();
			vStringPut (token->string, c);
			if (d == c)
			{
				vStringPut (token->string, d);
				token->type = TOKEN_OPERATOR;
			}
			else
			{
				fileUngetc (d);
				token->type = c;
			}
			break;
		}

		case '+':
		case '-':
		case '*':
		case '%':
		case '<':
		case '>':
		case '/':
		{
			int d = fileGetc ();
			vStringPut (token->string, c);
			if (d != '=')
				fileUngetc (d);
			else
				vStringPut (token->string, d);
			token->type = TOKEN_OPERATOR;
			break;
		}

		/* eats newline to implement line continuation  */
		case '\\':
		{
			int d = fileGetc ();
			if (d == '\r')
				d = fileGetc ();
			if (d != '\n')
				fileUngetc (d);
			goto getNextChar;
			break;
		}

		case '#': /* comment */
		case '\r': /* newlines for indent */
		case '\n':
			do
			{
				if (c == '#')
				{
					do
						c = fileGetc ();
					while (c != EOF && c != '\r' && c != '\n');
				}
				if (c == '\r')
				{
					int d = fileGetc ();
					if (d != '\n')
						fileUngetc (d);
				}
				token->type = TOKEN_INDENT;
				token->indent = 0;
				while ((c = fileGetc ()) == ' ' || c == '\t' || c == '\f')
				{
					if (c == '\t')
						token->indent += 8;
					else if (c == '\f') /* yeah, it's weird */
						token->indent = 0;
					else
						token->indent++;
				}
			} /* skip completely empty lines, so retry */
			while (c == '\r' || c == '\n' || c == '#');
			fileUngetc (c);
			break;

		default:
			if (! isIdentifierChar (c))
			{
				vStringPut (token->string, c);
				token->type = c;
			}
			else
			{
				/* FIXME: handle U, B and R string prefixes? */
				readIdentifier (token->string, c);
				token->keyword = analyzeToken (token->string, Lang_python);
				if (token->keyword == KEYWORD_NONE)
					token->type = TOKEN_IDENTIFIER;
				else
					token->type = TOKEN_KEYWORD;
			}
			break;
	}
}

static void readToken (tokenInfo *const token)
{
	readTokenFull (token, FALSE);
}

/*================================= parsing =================================*/


static boolean skipOverPair (tokenInfo *const token, int tOpen, int tClose,
                             vString *const repr)
{
	if (token->type == tOpen)
	{
		int depth = 1;

		if (repr)
			vStringCat (repr, token->string);
		do
		{
			readTokenFull (token, TRUE);
			if (repr)
			{
				if ((token->type != TOKEN_INDENT &&
				     token->type != TOKEN_WHITESPACE) ||
				    vStringLast (repr) != ' ')
				{
					vStringCat (repr, token->string);
				}
			}
			if (token->type == tOpen)
				depth ++;
			else if (token->type == tClose)
				depth --;
		}
		while (token->type != TOKEN_EOF && depth > 0);
	}
	if (repr)
		vStringTerminate (repr);

	return token->type == tClose;
}

static boolean skipLambdaArglist (tokenInfo *const token, vString *const repr)
{
	while (token->type != TOKEN_EOF && token->type != ':' &&
	       /* avoid reading too much, just in case */
	       token->type != TOKEN_INDENT)
	{
		boolean readNext = TRUE;

		if (token->type == '(')
			readNext = skipOverPair (token, '(', ')', repr);
		else if (token->type == '[')
			readNext = skipOverPair (token, '[', ']', repr);
		else if (token->type == '{')
			readNext = skipOverPair (token, '{', '}', repr);
		else if (token->keyword == KEYWORD_lambda)
		{ /* handle lambdas in a default value */
			if (repr)
				vStringCat (repr, token->string);
			readTokenFull (token, TRUE);
			readNext = skipLambdaArglist (token, repr);
			if (token->type == ':')
				readNext = TRUE;
			if (readNext && repr)
				vStringCat (repr, token->string);
		}
		else if (repr && (token->type != TOKEN_WHITESPACE ||
		                  vStringLast (repr) != ' '))
		{
			vStringCat (repr, token->string);
		}

		if (readNext)
			readTokenFull (token, TRUE);
	}
	return FALSE;
}

static boolean skipQualifiedName (tokenInfo *const token)
{
	if (token->type == TOKEN_IDENTIFIER)
	{
		readToken (token);
		while (token->type == '.')
		{
			readToken (token);
			if (token->type == TOKEN_IDENTIFIER)
				readToken (token);
		}
	}
	return FALSE;
}

static boolean readCDefName (tokenInfo *const token, pythonKind *kind)
{
	readToken (token);

	if (token->keyword == KEYWORD_extern ||
	    token->keyword == KEYWORD_import)
	{
		readToken (token);
		if (token->keyword == KEYWORD_from)
			return FALSE;
	}

	if (token->keyword == KEYWORD_class)
	{
		*kind = K_CLASS;
		readToken (token);
	}
	else
	{ /* skip type declaration */
		if (token->keyword == KEYWORD_inline)
			readToken (token);
		else
		{ /* otherwise, skip the optional type -- everything on the same line
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
					if (skipOverPair (token, '[', ']', NULL))
						readToken (token);
				}
				else if (token->type == '(')
				{
					if (skipOverPair (token, '(', ')', NULL))
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
	}

	return token->type == TOKEN_IDENTIFIER;
}

static boolean parseClassOrDef (tokenInfo *const token, pythonKind kind,
                                boolean isCDef)
{
	vString *arglist = NULL;
	tokenInfo *name = NULL;

	if (isCDef)
	{
		if (! readCDefName (token, &kind))
			return FALSE;
	}
	else
	{
		readToken (token);
		if (token->type != TOKEN_IDENTIFIER)
			return FALSE;
	}

	name = newToken ();
	copyToken (name, token);

	readToken (token);
	if (token->type == '(')
	{
		arglist = vStringNew ();
		skipOverPair (token, '(', ')', arglist);
	}

	if (kind == K_CLASS)
		makeClassTag (name, arglist);
	else
		makeFunctionTag (name, arglist);

	nestingLevelsPush (PythonNestingLevels, name->string, kind);
	nestingLevelsGetCurrent (PythonNestingLevels)->indentation = token->indent;

	deleteToken (name);
	vStringDelete (arglist);

	return TRUE;
}

/* pops any level >= to indent */
static void setIndent (int indent)
{
	NestingLevel *lv = nestingLevelsGetCurrent (PythonNestingLevels);

	while (lv && lv->indentation >= indent)
	{
		nestingLevelsPop (PythonNestingLevels);
		lv = nestingLevelsGetCurrent (PythonNestingLevels);
	}
}

static void findPythonTags (void)
{
	tokenInfo *const token = newToken ();
	boolean atLineStart = TRUE;

	NextToken = NULL;
	PythonNestingLevels = nestingLevelsNew ();

	readToken (token);
	while (token->type != TOKEN_EOF)
	{
		boolean readNext = TRUE;

		if (token->type == TOKEN_INDENT)
			setIndent (token->indent);
		else if (token->keyword == KEYWORD_class ||
		         token->keyword == KEYWORD_def)
		{
			pythonKind kind = token->keyword == KEYWORD_class ? K_CLASS : K_FUNCTION;

			readNext = parseClassOrDef (token, kind, FALSE);
		}
		else if (token->keyword == KEYWORD_cdef ||
		         token->keyword == KEYWORD_cpdef)
		{
			readNext = parseClassOrDef (token, K_FUNCTION, TRUE);
		}
		else if (token->keyword == KEYWORD_import)
		{
			do
			{
				readToken (token);
				if (token->type == TOKEN_IDENTIFIER)
				{
					tokenInfo *name = newToken ();

					copyToken (name, token);
					/* skip sub-levels in foo.bar.baz */
					skipQualifiedName (token);
					/* if there is an "as", use it as the name */
					if (token->keyword == KEYWORD_as)
					{
						readToken (token);
						if (token->type == TOKEN_IDENTIFIER)
						{
							copyToken (name, token);
							readToken (token);
						}
					}

					makeSimplePythonTag (name, K_IMPORT);
				}
			}
			while (token->type == ',');
			readNext = FALSE;
		}
		else if (token->type == '(')
		{ /* skip parentheses to avoid finding stuff inside them */
			readNext = skipOverPair (token, '(', ')', NULL);
		}
		else if (token->type == TOKEN_IDENTIFIER && atLineStart)
		{
			NestingLevel *lv = nestingLevelsGetCurrent (PythonNestingLevels);

			if (! lv || lv->type == K_CLASS)
			{
				tokenInfo *name = newToken ();

				do
				{
					copyToken (name, token);
					readToken (token);
					/* FIXME: to get perfect tag types, we'd need to collect
					 *        the multiple names, and then map the initializers
					 *        back, but that's very hard. */
					if (token->type == ',')
					{
						makeSimplePythonTag (name, K_VARIABLE);
						readToken (token);
						if (token->type != TOKEN_IDENTIFIER)
						{
							readNext = FALSE;
							break;
						}
					}
					else
					{
						if (token->type == '=')
						{
							/* check for lambdas */
							readToken (token);
							readNext = FALSE;
							if (token->keyword != KEYWORD_lambda)
								makeSimplePythonTag (name, K_VARIABLE);
							else
							{
								vString *arglist = vStringNew ();

								readToken (token);
								vStringPut (arglist, '(');
								readNext = skipLambdaArglist (token, arglist);
								vStringPut (arglist, ')');
								makeFunctionTag (name, arglist);
							}
						}
						break;
					}
				}
				while (token->type == TOKEN_IDENTIFIER);

				deleteToken (name);
			}
		}

		atLineStart = token->type == TOKEN_INDENT;

		if (readNext)
			readToken (token);
	}

	nestingLevelsFree (PythonNestingLevels);
	deleteToken (token);
	Assert (NextToken == NULL);
}

static void initialize (const langType language)
{
	Lang_python = language;
	buildPythonKeywordHash ();
}

extern parserDefinition* PythonParser (void)
{
	static const char *const extensions[] = { "py", "pyx", "pxd", "pxi", "scons", NULL };
	static const char *const aliases[] = { "python[23]*", "scons", NULL };
	parserDefinition *def = parserNew ("Python");
	def->kinds = PythonKinds;
	def->kindCount = KIND_COUNT (PythonKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser = findPythonTags;
	def->initialize = initialize;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
