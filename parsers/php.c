/*
*   Copyright (c) 2013, Colomban Wendling <ban@herbesfolles.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains code for generating tags for the PHP scripting
*   language.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "keyword.h"
#include "entry.h"
#include "routines.h"
#include "debug.h"


typedef enum {
	KEYWORD_abstract,
	KEYWORD_and,
	KEYWORD_as,
	KEYWORD_break,
	KEYWORD_callable,
	KEYWORD_case,
	KEYWORD_catch,
	KEYWORD_class,
	KEYWORD_clone,
	KEYWORD_const,
	KEYWORD_continue,
	KEYWORD_declare,
	KEYWORD_define,
	KEYWORD_default,
	KEYWORD_do,
	KEYWORD_echo,
	KEYWORD_else,
	KEYWORD_elif,
	KEYWORD_enddeclare,
	KEYWORD_endfor,
	KEYWORD_endforeach,
	KEYWORD_endif,
	KEYWORD_endswitch,
	KEYWORD_endwhile,
	KEYWORD_extends,
	KEYWORD_final,
	KEYWORD_finally,
	KEYWORD_for,
	KEYWORD_foreach,
	KEYWORD_function,
	KEYWORD_global,
	KEYWORD_goto,
	KEYWORD_if,
	KEYWORD_implements,
	KEYWORD_include,
	KEYWORD_include_once,
	KEYWORD_instanceof,
	KEYWORD_insteadof,
	KEYWORD_interface,
	KEYWORD_namespace,
	KEYWORD_new,
	KEYWORD_or,
	KEYWORD_print,
	KEYWORD_private,
	KEYWORD_protected,
	KEYWORD_public,
	KEYWORD_require,
	KEYWORD_require_once,
	KEYWORD_return,
	KEYWORD_static,
	KEYWORD_switch,
	KEYWORD_throw,
	KEYWORD_trait,
	KEYWORD_try,
	KEYWORD_use,
	KEYWORD_var,
	KEYWORD_while,
	KEYWORD_xor,
	KEYWORD_yield
} keywordId;

typedef enum {
	ACCESS_UNDEFINED,
	ACCESS_PRIVATE,
	ACCESS_PROTECTED,
	ACCESS_PUBLIC,
	COUNT_ACCESS
} accessType;

typedef enum {
	IMPL_UNDEFINED,
	IMPL_ABSTRACT,
	COUNT_IMPL
} implType;

typedef enum {
	K_CLASS,
	K_DEFINE,
	K_FUNCTION,
	K_INTERFACE,
	K_LOCAL_VARIABLE,
	K_NAMESPACE,
	K_TRAIT,
	K_VARIABLE,
	K_ALIAS,
	COUNT_KIND
} phpKind;

#define NAMESPACE_SEPARATOR "\\"
static scopeSeparator PhpGenericSeparators [] = {
	{ 'n'          , NAMESPACE_SEPARATOR },
	{ KIND_WILDCARD, "::" },
};

static kindOption PhpKinds[COUNT_KIND] = {
	{ TRUE, 'c', "class",		"classes",
	  ATTACH_SEPARATORS(PhpGenericSeparators) },
	{ TRUE, 'd', "define",		"constant definitions",
	  ATTACH_SEPARATORS(PhpGenericSeparators)},
	{ TRUE, 'f', "function",	"functions",
	  ATTACH_SEPARATORS(PhpGenericSeparators)},
	{ TRUE, 'i', "interface",	"interfaces",
	  ATTACH_SEPARATORS(PhpGenericSeparators)},
	{ FALSE, 'l', "local",		"local variables",
	  ATTACH_SEPARATORS(PhpGenericSeparators)},
	{ TRUE, 'n', "namespace",	"namespaces",
	  ATTACH_SEPARATORS(PhpGenericSeparators)},
	{ TRUE, 't', "trait",		"traits",
	  ATTACH_SEPARATORS(PhpGenericSeparators)},
	{ TRUE, 'v', "variable",	"variables",
	  ATTACH_SEPARATORS(PhpGenericSeparators)},
	{ TRUE, 'a', "alias",		"aliases",
	  ATTACH_SEPARATORS(PhpGenericSeparators)},
};

static const keywordTable PhpKeywordTable[] = {
	/* keyword			keyword ID */
	{ "abstract",		KEYWORD_abstract		},
	{ "and",			KEYWORD_and				},
	{ "as",				KEYWORD_as				},
	{ "break",			KEYWORD_break			},
	{ "callable",		KEYWORD_callable		},
	{ "case",			KEYWORD_case			},
	{ "catch",			KEYWORD_catch			},
	{ "cfunction",		KEYWORD_function		}, /* nobody knows what the hell this is, but it seems to behave much like "function" so bind it to it */
	{ "class",			KEYWORD_class			},
	{ "clone",			KEYWORD_clone			},
	{ "const",			KEYWORD_const			},
	{ "continue",		KEYWORD_continue		},
	{ "declare",		KEYWORD_declare			},
	{ "define",			KEYWORD_define			}, /* this isn't really a keyword but we handle it so it's easier this way */
	{ "default",		KEYWORD_default			},
	{ "do",				KEYWORD_do				},
	{ "echo",			KEYWORD_echo			},
	{ "else",			KEYWORD_else			},
	{ "elseif",			KEYWORD_elif			},
	{ "enddeclare",		KEYWORD_enddeclare		},
	{ "endfor",			KEYWORD_endfor			},
	{ "endforeach",		KEYWORD_endforeach		},
	{ "endif",			KEYWORD_endif			},
	{ "endswitch",		KEYWORD_endswitch		},
	{ "endwhile",		KEYWORD_endwhile		},
	{ "extends",		KEYWORD_extends			},
	{ "final",			KEYWORD_final			},
	{ "finally",		KEYWORD_finally			},
	{ "for",			KEYWORD_for				},
	{ "foreach",		KEYWORD_foreach			},
	{ "function",		KEYWORD_function		},
	{ "global",			KEYWORD_global			},
	{ "goto",			KEYWORD_goto			},
	{ "if",				KEYWORD_if				},
	{ "implements",		KEYWORD_implements		},
	{ "include",		KEYWORD_include			},
	{ "include_once",	KEYWORD_include_once	},
	{ "instanceof",		KEYWORD_instanceof		},
	{ "insteadof",		KEYWORD_insteadof		},
	{ "interface",		KEYWORD_interface		},
	{ "namespace",		KEYWORD_namespace		},
	{ "new",			KEYWORD_new				},
	{ "or",				KEYWORD_or				},
	{ "print",			KEYWORD_print			},
	{ "private",		KEYWORD_private			},
	{ "protected",		KEYWORD_protected		},
	{ "public",			KEYWORD_public			},
	{ "require",		KEYWORD_require			},
	{ "require_once",	KEYWORD_require_once	},
	{ "return",			KEYWORD_return			},
	{ "static",			KEYWORD_static			},
	{ "switch",			KEYWORD_switch			},
	{ "throw",			KEYWORD_throw			},
	{ "trait",			KEYWORD_trait			},
	{ "try",			KEYWORD_try				},
	{ "use",			KEYWORD_use				},
	{ "var",			KEYWORD_var				},
	{ "while",			KEYWORD_while			},
	{ "xor",			KEYWORD_xor				},
	{ "yield",			KEYWORD_yield			}
};


typedef enum eTokenType {
	TOKEN_UNDEFINED,
	TOKEN_EOF,
	TOKEN_CHARACTER,
	TOKEN_CLOSE_PAREN,
	TOKEN_SEMICOLON,
	TOKEN_COLON,
	TOKEN_COMMA,
	TOKEN_KEYWORD,
	TOKEN_OPEN_PAREN,
	TOKEN_OPERATOR,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
	TOKEN_PERIOD,
	TOKEN_OPEN_CURLY,
	TOKEN_CLOSE_CURLY,
	TOKEN_EQUAL_SIGN,
	TOKEN_OPEN_SQUARE,
	TOKEN_CLOSE_SQUARE,
	TOKEN_VARIABLE,
	TOKEN_AMPERSAND,
	TOKEN_BACKSLASH
} tokenType;

typedef struct {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	vString *		scope;
	unsigned long 	lineNumber;
	MIOPos			filePosition;
	int 			parentKind; /* -1 if none */
} tokenInfo;

static langType Lang_php;
static langType Lang_zephir;

static boolean InPhp = FALSE; /* whether we are between <? ?> */

/* current statement details */
static struct {
	accessType access;
	implType impl;
} CurrentStatement;

/* Current namespace */
static vString *CurrentNamesapce;
/* Cache variable to build the tag's scope.  It has no real meaning outside
 * of initPhpEntry()'s scope. */
static vString *FullScope;
/* Anonymous symbol count to generate file-unique names */
static unsigned int AnonymousID;

static const char *phpScopeSeparatorFor (int phpKind,
					 int phpUpperScopeKind)
{
	char letter;

	letter = PhpKinds[phpUpperScopeKind].letter;
	return scopeSeparatorFor (&(PhpKinds[phpKind]), letter);
}

static const char *accessToString (const accessType access)
{
	static const char *const names[COUNT_ACCESS] = {
		"undefined",
		"private",
		"protected",
		"public"
	};

	Assert (access < COUNT_ACCESS);

	return names[access];
}

static const char *implToString (const implType impl)
{
	static const char *const names[COUNT_IMPL] = {
		"undefined",
		"abstract"
	};

	Assert (impl < COUNT_IMPL);

	return names[impl];
}

static void initPhpEntry (tagEntryInfo *const e, const tokenInfo *const token,
						  const phpKind kind, const accessType access)
{
	int parentKind = -1;

	vStringClear (FullScope);

	if (vStringLength (CurrentNamesapce) > 0)
	{
		parentKind = K_NAMESPACE;
		vStringCat (FullScope, CurrentNamesapce);

	}

	initTagEntry (e, vStringValue (token->string), &(PhpKinds[kind]));

	e->lineNumber	= token->lineNumber;
	e->filePosition	= token->filePosition;

	if (access != ACCESS_UNDEFINED)
		e->extensionFields.access = accessToString (access);
	if (vStringLength (token->scope) > 0)
	{
		parentKind = token->parentKind;

		if (vStringLength (FullScope) > 0)
		{
			const char* sep;

			sep = phpScopeSeparatorFor (parentKind,
						    K_NAMESPACE);
			vStringCatS (FullScope, sep);
		}
			vStringCat (FullScope, token->scope);
	}
	if (vStringLength (FullScope) > 0)
	{
		Assert (parentKind >= 0);

		vStringTerminate (FullScope);
		e->extensionFields.scopeKind = &(PhpKinds[parentKind]);
		e->extensionFields.scopeName = vStringValue (FullScope);
	}
}

static void  makePhpTagEntry  (tagEntryInfo *const e)
{
	makeTagEntry (e);
	makeQualifiedTagEntry (e);
}
static void makeSimplePhpTag (const tokenInfo *const token, const phpKind kind,
							  const accessType access)
{
	if (PhpKinds[kind].enabled)
	{
		tagEntryInfo e;

		initPhpEntry (&e, token, kind, access);
		makePhpTagEntry (&e);
	}
}

static void makeNamespacePhpTag (const tokenInfo *const token, const vString *const name)
{
	if (PhpKinds[K_NAMESPACE].enabled)
	{
		tagEntryInfo e;

		initTagEntry (&e, vStringValue (name), &(PhpKinds[K_NAMESPACE]));

		e.lineNumber	= token->lineNumber;
		e.filePosition	= token->filePosition;

		makePhpTagEntry (&e);
	}
}

static void makeClassOrIfaceTag (const phpKind kind, const tokenInfo *const token,
								 vString *const inheritance, const implType impl)
{
	if (PhpKinds[kind].enabled)
	{
		tagEntryInfo e;

		initPhpEntry (&e, token, kind, ACCESS_UNDEFINED);

		if (impl != IMPL_UNDEFINED)
			e.extensionFields.implementation = implToString (impl);
		if (vStringLength (inheritance) > 0)
			e.extensionFields.inheritance = vStringValue (inheritance);

		makePhpTagEntry (&e);
	}
}

static void makeFunctionTag (const tokenInfo *const token,
							 const vString *const arglist,
							 const accessType access, const implType impl)
{
	if (PhpKinds[K_FUNCTION].enabled)
	{
		tagEntryInfo e;

		initPhpEntry (&e, token, K_FUNCTION, access);

		if (impl != IMPL_UNDEFINED)
			e.extensionFields.implementation = implToString (impl);
		if (arglist)
			e.extensionFields.signature = vStringValue (arglist);

		makePhpTagEntry (&e);
	}
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->string		= vStringNew ();
	token->scope		= vStringNew ();
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
	token->parentKind	= -1;

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	vStringDelete (token->scope);
	eFree (token);
}

static void copyToken (tokenInfo *const dest, const tokenInfo *const src,
					   boolean scope)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	vStringCopy(dest->string, src->string);
	dest->parentKind = src->parentKind;
	if (scope)
		vStringCopy(dest->scope, src->scope);
}

#if 0
#include <stdio.h>

static const char *tokenTypeName (const tokenType type)
{
	switch (type)
	{
		case TOKEN_UNDEFINED:		return "undefined";
		case TOKEN_EOF:				return "EOF";
		case TOKEN_CHARACTER:		return "character";
		case TOKEN_CLOSE_PAREN:		return "')'";
		case TOKEN_SEMICOLON:		return "';'";
		case TOKEN_COLON:			return "':'";
		case TOKEN_COMMA:			return "','";
		case TOKEN_OPEN_PAREN:		return "'('";
		case TOKEN_OPERATOR:		return "operator";
		case TOKEN_IDENTIFIER:		return "identifier";
		case TOKEN_KEYWORD:			return "keyword";
		case TOKEN_STRING:			return "string";
		case TOKEN_PERIOD:			return "'.'";
		case TOKEN_OPEN_CURLY:		return "'{'";
		case TOKEN_CLOSE_CURLY:		return "'}'";
		case TOKEN_EQUAL_SIGN:		return "'='";
		case TOKEN_OPEN_SQUARE:		return "'['";
		case TOKEN_CLOSE_SQUARE:	return "']'";
		case TOKEN_VARIABLE:		return "variable";
	}
	return NULL;
}

static void printToken (const tokenInfo *const token)
{
	fprintf (stderr, "%p:\n\ttype:\t%s\n\tline:\t%lu\n\tscope:\t%s\n", (void *) token,
			 tokenTypeName (token->type),
			 token->lineNumber,
			 vStringValue (token->scope));
	switch (token->type)
	{
		case TOKEN_IDENTIFIER:
		case TOKEN_STRING:
		case TOKEN_VARIABLE:
			fprintf (stderr, "\tcontent:\t%s\n", vStringValue (token->string));
			break;

		case TOKEN_KEYWORD:
		{
			size_t n = ARRAY_SIZE (PhpKeywordTable);
			size_t i;

			fprintf (stderr, "\tkeyword:\t");
			for (i = 0; i < n; i++)
			{
				if (PhpKeywordTable[i].id == token->keyword)
				{
					fprintf (stderr, "%s\n", PhpKeywordTable[i].name);
					break;
				}
			}
			if (i >= n)
				fprintf (stderr, "(unknown)\n");
		}

		default: break;
	}
}
#endif

static void addToScope (tokenInfo *const token, const vString *const extra,
			int kindOfUpperScope)
{
	if (vStringLength (token->scope) > 0)
	{
		const char* sep;

		sep = phpScopeSeparatorFor(token->parentKind,
					   kindOfUpperScope);
		vStringCatS (token->scope, sep);
	}
	vStringCat (token->scope, extra);
	vStringTerminate(token->scope);
}

static boolean isIdentChar (const int c)
{
	return (isalnum (c) || c == '_' || c >= 0x80);
}

static int skipToCharacter (const int c)
{
	int d;
	do
	{
		d = getcFromInputFile ();
	} while (d != EOF  &&  d != c);
	return d;
}

static void parseString (vString *const string, const int delimiter)
{
	while (TRUE)
	{
		int c = getcFromInputFile ();

		if (c == '\\' && (c = getcFromInputFile ()) != EOF)
			vStringPut (string, (char) c);
		else if (c == EOF || c == delimiter)
			break;
		else
			vStringPut (string, (char) c);
	}
	vStringTerminate (string);
}

/* reads an HereDoc or a NowDoc (the part after the <<<).
 * 	<<<[ \t]*(ID|'ID'|"ID")
 * 	...
 * 	ID;?
 *
 * note that:
 *  1) starting ID must be immediately followed by a newline;
 *  2) closing ID is the same as opening one;
 *  3) closing ID must be immediately followed by a newline or a semicolon
 *     then a newline.
 *
 * Example of a *single* valid heredoc:
 * 	<<< FOO
 * 	something
 * 	something else
 * 	FOO this is not an end
 * 	FOO; this isn't either
 * 	FOO; # neither this is
 * 	FOO;
 * 	# previous line was the end, but the semicolon wasn't required
 */
static void parseHeredoc (vString *const string)
{
	int c;
	unsigned int len;
	char delimiter[64]; /* arbitrary limit, but more is crazy anyway */
	int quote = 0;

	do
	{
		c = getcFromInputFile ();
	}
	while (c == ' ' || c == '\t');

	if (c == '\'' || c == '"')
	{
		quote = c;
		c = getcFromInputFile ();
	}
	for (len = 0; len < ARRAY_SIZE (delimiter) - 1; len++)
	{
		if (! isIdentChar (c))
			break;
		delimiter[len] = (char) c;
		c = getcFromInputFile ();
	}
	delimiter[len] = 0;

	if (len == 0) /* no delimiter, give up */
		goto error;
	if (quote)
	{
		if (c != quote) /* no closing quote for quoted identifier, give up */
			goto error;
		c = getcFromInputFile ();
	}
	if (c != '\r' && c != '\n') /* missing newline, give up */
		goto error;

	do
	{
		c = getcFromInputFile ();

		if (c != '\r' && c != '\n')
			vStringPut (string, (char) c);
		else
		{
			/* new line, check for a delimiter right after */
			int nl = c;
			int extra = EOF;

			c = getcFromInputFile ();
			for (len = 0; c != 0 && (c - delimiter[len]) == 0; len++)
				c = getcFromInputFile ();

			if (delimiter[len] != 0)
				ungetcToInputFile (c);
			else
			{
				/* line start matched the delimiter, now check whether there
				 * is anything after it */
				if (c == '\r' || c == '\n')
				{
					ungetcToInputFile (c);
					break;
				}
				else if (c == ';')
				{
					int d = getcFromInputFile ();
					if (d == '\r' || d == '\n')
					{
						/* put back the semicolon since it's not part of the
						 * string.  we can't put back the newline, but it's a
						 * whitespace character nobody cares about it anyway */
						ungetcToInputFile (';');
						break;
					}
					else
					{
						/* put semicolon in the string and continue */
						extra = ';';
						ungetcToInputFile (d);
					}
				}
			}
			/* if we are here it wasn't a delimiter, so put everything in the
			 * string */
			vStringPut (string, (char) nl);
			vStringNCatS (string, delimiter, len);
			if (extra != EOF)
				vStringPut (string, (char) extra);
		}
	}
	while (c != EOF);

	vStringTerminate (string);

	return;

error:
	ungetcToInputFile (c);
}

static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	do
	{
		vStringPut (string, (char) c);
		c = getcFromInputFile ();
	} while (isIdentChar (c));
	ungetcToInputFile (c);
	vStringTerminate (string);
}

static boolean isSpace (int c)
{
	return (c == '\t' || c == ' ' || c == '\v' ||
			c == '\n' || c == '\r' || c == '\f');
}

static int skipWhitespaces (int c)
{
	while (isSpace (c))
		c = getcFromInputFile ();
	return c;
}

/* <script[:white:]+language[:white:]*=[:white:]*(php|'php'|"php")[:white:]*>
 * 
 * This is ugly, but the whole "<script language=php>" tag is and we can't
 * really do better without adding a lot of code only for this */
static boolean isOpenScriptLanguagePhp (int c)
{
	int quote = 0;

	/* <script[:white:]+language[:white:]*= */
	if (c                                   != '<' ||
		tolower ((c = getcFromInputFile ()))         != 's' ||
		tolower ((c = getcFromInputFile ()))         != 'c' ||
		tolower ((c = getcFromInputFile ()))         != 'r' ||
		tolower ((c = getcFromInputFile ()))         != 'i' ||
		tolower ((c = getcFromInputFile ()))         != 'p' ||
		tolower ((c = getcFromInputFile ()))         != 't' ||
		! isSpace ((c = getcFromInputFile ()))              ||
		tolower ((c = skipWhitespaces (c))) != 'l' ||
		tolower ((c = getcFromInputFile ()))         != 'a' ||
		tolower ((c = getcFromInputFile ()))         != 'n' ||
		tolower ((c = getcFromInputFile ()))         != 'g' ||
		tolower ((c = getcFromInputFile ()))         != 'u' ||
		tolower ((c = getcFromInputFile ()))         != 'a' ||
		tolower ((c = getcFromInputFile ()))         != 'g' ||
		tolower ((c = getcFromInputFile ()))         != 'e' ||
		(c = skipWhitespaces (getcFromInputFile ())) != '=')
		return FALSE;

	/* (php|'php'|"php")> */
	c = skipWhitespaces (getcFromInputFile ());
	if (c == '"' || c == '\'')
	{
		quote = c;
		c = getcFromInputFile ();
	}
	if (tolower (c)                         != 'p' ||
		tolower ((c = getcFromInputFile ()))         != 'h' ||
		tolower ((c = getcFromInputFile ()))         != 'p' ||
		(quote != 0 && (c = getcFromInputFile ()) != quote) ||
		(c = skipWhitespaces (getcFromInputFile ())) != '>')
		return FALSE;

	return TRUE;
}

static int findPhpStart (void)
{
	int c;
	do
	{
		if ((c = getcFromInputFile ()) == '<')
		{
			c = getcFromInputFile ();
			/* <? and <?php, but not <?xml */
			if (c == '?')
			{
				/* don't enter PHP mode on "<?xml", yet still support short open tags (<?) */
				if (tolower ((c = getcFromInputFile ())) != 'x' ||
					tolower ((c = getcFromInputFile ())) != 'm' ||
					tolower ((c = getcFromInputFile ())) != 'l')
				{
					break;
				}
			}
			/* <script language="php"> */
			else
			{
				ungetcToInputFile (c);
				if (isOpenScriptLanguagePhp ('<'))
					break;
			}
		}
	}
	while (c != EOF);

	return c;
}

static int skipSingleComment (void)
{
	int c;
	do
	{
		c = getcFromInputFile ();
		if (c == '\r')
		{
			int next = getcFromInputFile ();
			if (next != '\n')
				ungetcToInputFile (next);
			else
				c = next;
		}
		/* ?> in single-line comments leaves PHP mode */
		else if (c == '?')
		{
			int next = getcFromInputFile ();
			if (next == '>')
				InPhp = FALSE;
			else
				ungetcToInputFile (next);
		}
	} while (InPhp && c != EOF && c != '\n' && c != '\r');
	return c;
}

static void readToken (tokenInfo *const token)
{
	int c;

	token->type		= TOKEN_UNDEFINED;
	token->keyword	= KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:

	if (! InPhp)
	{
		c = findPhpStart ();
		if (c != EOF)
			InPhp = TRUE;
	}
	else
		c = getcFromInputFile ();

	c = skipWhitespaces (c);

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	switch (c)
	{
		case EOF: token->type = TOKEN_EOF;					break;
		case '(': token->type = TOKEN_OPEN_PAREN;			break;
		case ')': token->type = TOKEN_CLOSE_PAREN;			break;
		case ';': token->type = TOKEN_SEMICOLON;			break;
		case ',': token->type = TOKEN_COMMA;				break;
		case '.': token->type = TOKEN_PERIOD;				break;
		case ':': token->type = TOKEN_COLON;				break;
		case '{': token->type = TOKEN_OPEN_CURLY;			break;
		case '}': token->type = TOKEN_CLOSE_CURLY;			break;
		case '[': token->type = TOKEN_OPEN_SQUARE;			break;
		case ']': token->type = TOKEN_CLOSE_SQUARE;			break;
		case '&': token->type = TOKEN_AMPERSAND;			break;
		case '\\': token->type = TOKEN_BACKSLASH;			break;

		case '=':
		{
			int d = getcFromInputFile ();
			if (d == '=' || d == '>')
				token->type = TOKEN_OPERATOR;
			else
			{
				ungetcToInputFile (d);
				token->type = TOKEN_EQUAL_SIGN;
			}
			break;
		}

		case '\'':
		case '"':
			token->type = TOKEN_STRING;
			parseString (token->string, c);
			token->lineNumber = getInputLineNumber ();
			token->filePosition = getInputFilePosition ();
			break;

		case '<':
		{
			int d = getcFromInputFile ();
			if (d == '/')
			{
				/* </script[:white:]*> */
				if (tolower ((d = getcFromInputFile ())) == 's' &&
					tolower ((d = getcFromInputFile ())) == 'c' &&
					tolower ((d = getcFromInputFile ())) == 'r' &&
					tolower ((d = getcFromInputFile ())) == 'i' &&
					tolower ((d = getcFromInputFile ())) == 'p' &&
					tolower ((d = getcFromInputFile ())) == 't' &&
					(d = skipWhitespaces (getcFromInputFile ())) == '>')
				{
					InPhp = FALSE;
					goto getNextChar;
				}
				else
				{
					ungetcToInputFile (d);
					token->type = TOKEN_UNDEFINED;
				}
			}
			else if (d == '<' && (d = getcFromInputFile ()) == '<')
			{
				token->type = TOKEN_STRING;
				parseHeredoc (token->string);
			}
			else
			{
				ungetcToInputFile (d);
				token->type = TOKEN_UNDEFINED;
			}
			break;
		}

		case '#': /* comment */
			skipSingleComment ();
			goto getNextChar;
			break;

		case '+':
		case '-':
		case '*':
		case '%':
		{
			int d = getcFromInputFile ();
			if (d != '=' && ! (c == '-' && d == '>'))
				ungetcToInputFile (d);
			token->type = TOKEN_OPERATOR;
			break;
		}

		case '/': /* division or comment start */
		{
			int d = getcFromInputFile ();
			if (d == '/') /* single-line comment */
			{
				skipSingleComment ();
				goto getNextChar;
			}
			else if (d == '*')
			{
				do
				{
					c = skipToCharacter ('*');
					if (c != EOF)
					{
						c = getcFromInputFile ();
						if (c == '/')
							break;
						else
							ungetcToInputFile (c);
					}
				} while (c != EOF && c != '\0');
				goto getNextChar;
			}
			else
			{
				if (d != '=')
					ungetcToInputFile (d);
				token->type = TOKEN_OPERATOR;
			}
			break;
		}

		case '$': /* variable start */
		{
			int d = getcFromInputFile ();
			if (! isIdentChar (d))
			{
				ungetcToInputFile (d);
				token->type = TOKEN_UNDEFINED;
			}
			else
			{
				parseIdentifier (token->string, d);
				token->type = TOKEN_VARIABLE;
			}
			break;
		}

		case '?': /* maybe the end of the PHP chunk */
		{
			int d = getcFromInputFile ();
			if (d == '>')
			{
				InPhp = FALSE;
				goto getNextChar;
			}
			else
			{
				ungetcToInputFile (d);
				token->type = TOKEN_UNDEFINED;
			}
			break;
		}

		default:
			if (! isIdentChar (c))
				token->type = TOKEN_UNDEFINED;
			else
			{
				parseIdentifier (token->string, c);
				token->keyword = analyzeToken (token->string, getInputLanguage ());
				if (token->keyword == KEYWORD_NONE)
					token->type = TOKEN_IDENTIFIER;
				else
					token->type = TOKEN_KEYWORD;
			}
			break;
	}

	if (token->type == TOKEN_SEMICOLON ||
		token->type == TOKEN_OPEN_CURLY ||
		token->type == TOKEN_CLOSE_CURLY)
	{
		/* reset current statement details on statement end, and when entering
		 * a deeper scope.
		 * it is a bit ugly to do this in readToken(), but it makes everything
		 * a lot simpler. */
		CurrentStatement.access = ACCESS_UNDEFINED;
		CurrentStatement.impl = IMPL_UNDEFINED;
	}
}

static void enterScope (tokenInfo *const parentToken,
						const vString *const extraScope,
						const int parentKind);

static void skipOverParens (tokenInfo *token)
{
	if (token->type == TOKEN_OPEN_PAREN)
	{
		int depth = 1;

		do
		{
			readToken (token);
			switch (token->type)
			{
				case TOKEN_OPEN_PAREN:  depth++; break;
				case TOKEN_CLOSE_PAREN: depth--; break;
				default: break;
			}
		}
		while (token->type != TOKEN_EOF && depth > 0);

		readToken (token);
	}
}

/* parses a class or an interface:
 * 	class Foo {}
 * 	class Foo extends Bar {}
 * 	class Foo extends Bar implements iFoo, iBar {}
 * 	interface iFoo {}
 * 	interface iBar extends iFoo {}
 *
 * if @name is not NULL, parses an anonymous class with name @name
 * 	new class {}
 * 	new class(1, 2) {}
 * 	new class(1, 2) extends Foo implements iFoo, iBar {} */
static boolean parseClassOrIface (tokenInfo *const token, const phpKind kind,
                                  const tokenInfo *name)
{
	boolean readNext = TRUE;
	implType impl = CurrentStatement.impl;
	tokenInfo *nameFree = NULL;
	vString *inheritance = NULL;

	readToken (token);
	if (name) /* anonymous class */
	{
		/* skip possible construction arguments */
		skipOverParens (token);
	}
	else /* normal, named class */
	{
		if (token->type != TOKEN_IDENTIFIER)
			return FALSE;

		name = nameFree = newToken ();
		copyToken (nameFree, token, TRUE);

		readToken (token);
	}

	inheritance = vStringNew ();
	/* read every identifiers, keywords and commas, and assume each
	 *  identifier (not keyword) is an inheritance
	 * (like in "class Foo extends Bar implements iA, iB") */
	while (token->type == TOKEN_IDENTIFIER ||
	       token->type == TOKEN_KEYWORD ||
	       token->type == TOKEN_COMMA)
	{
		if (token->type == TOKEN_IDENTIFIER)
		{
			if (vStringLength (inheritance) > 0)
				vStringPut (inheritance, ',');
			vStringCat (inheritance, token->string);
		}

		readToken (token);
	}

	makeClassOrIfaceTag (kind, name, inheritance, impl);

	if (token->type == TOKEN_OPEN_CURLY)
		enterScope (token, name->string, kind);
	else
		readNext = FALSE;

	if (nameFree)
		deleteToken (nameFree);
	vStringDelete (inheritance);

	return readNext;
}

/* parses a trait:
 * 	trait Foo {} */
static boolean parseTrait (tokenInfo *const token)
{
	boolean readNext = TRUE;
	tokenInfo *name;

	readToken (token);
	if (token->type != TOKEN_IDENTIFIER)
		return FALSE;

	name = newToken ();
	copyToken (name, token, TRUE);

	makeSimplePhpTag (name, K_TRAIT, ACCESS_UNDEFINED);

	readToken (token);
	if (token->type == TOKEN_OPEN_CURLY)
		enterScope (token, name->string, K_TRAIT);
	else
		readNext = FALSE;

	deleteToken (name);

	return readNext;
}

/* parse a function
 *
 * if @name is NULL, parses a normal function
 * 	function myfunc($foo, $bar) {}
 * 	function &myfunc($foo, $bar) {}
 * 	function myfunc($foo, $bar) : type {}
 *
 * if @name is not NULL, parses an anonymous function with name @name
 * 	$foo = function($foo, $bar) {}
 * 	$foo = function&($foo, $bar) {}
 * 	$foo = function($foo, $bar) use ($x, &$y) {}
 * 	$foo = function($foo, $bar) use ($x, &$y) : type {} */
static boolean parseFunction (tokenInfo *const token, const tokenInfo *name)
{
	boolean readNext = TRUE;
	accessType access = CurrentStatement.access;
	implType impl = CurrentStatement.impl;
	tokenInfo *nameFree = NULL;

	readToken (token);
	/* skip a possible leading ampersand (return by reference) */
	if (token->type == TOKEN_AMPERSAND)
		readToken (token);

	if (! name)
	{
		if (token->type != TOKEN_IDENTIFIER && token->type != TOKEN_KEYWORD)
			return FALSE;

		name = nameFree = newToken ();
		copyToken (nameFree, token, TRUE);
		readToken (token);
	}

	if (token->type == TOKEN_OPEN_PAREN)
	{
		vString *arglist = vStringNew ();
		int depth = 1;

		vStringPut (arglist, '(');
		do
		{
			readToken (token);

			switch (token->type)
			{
				case TOKEN_OPEN_PAREN:  depth++; break;
				case TOKEN_CLOSE_PAREN: depth--; break;
				default: break;
			}
			/* display part */
			switch (token->type)
			{
				case TOKEN_AMPERSAND:		vStringPut (arglist, '&');		break;
				case TOKEN_CLOSE_CURLY:		vStringPut (arglist, '}');		break;
				case TOKEN_CLOSE_PAREN:		vStringPut (arglist, ')');		break;
				case TOKEN_CLOSE_SQUARE:	vStringPut (arglist, ']');		break;
				case TOKEN_COLON:			vStringPut (arglist, ':');		break;
				case TOKEN_COMMA:			vStringCatS (arglist, ", ");	break;
				case TOKEN_EQUAL_SIGN:		vStringCatS (arglist, " = ");	break;
				case TOKEN_OPEN_CURLY:		vStringPut (arglist, '{');		break;
				case TOKEN_OPEN_PAREN:		vStringPut (arglist, '(');		break;
				case TOKEN_OPEN_SQUARE:		vStringPut (arglist, '[');		break;
				case TOKEN_PERIOD:			vStringPut (arglist, '.');		break;
				case TOKEN_SEMICOLON:		vStringPut (arglist, ';');		break;
				case TOKEN_BACKSLASH:		vStringPut (arglist, '\\');		break;
				case TOKEN_STRING:
				{
					vStringCatS (arglist, "'");	
					vStringCat  (arglist, token->string);
					vStringCatS (arglist, "'");
					break;
				}

				case TOKEN_IDENTIFIER:
				case TOKEN_KEYWORD:
				case TOKEN_VARIABLE:
				{
					switch (vStringLast (arglist))
					{
						case 0:
						case ' ':
						case '{':
						case '(':
						case '[':
						case '.':
						case '\\':
							/* no need for a space between those and the identifier */
							break;

						default:
							vStringPut (arglist, ' ');
							break;
					}
					if (token->type == TOKEN_VARIABLE)
						vStringPut (arglist, '$');
					vStringCat (arglist, token->string);
					break;
				}

				default: break;
			}
		}
		while (token->type != TOKEN_EOF && depth > 0);

		vStringTerminate (arglist);

		makeFunctionTag (name, arglist, access, impl);
		vStringDelete (arglist);

		readToken (token); /* normally it's an open brace or "use" keyword */
	}

	/* skip use(...) */
	if (token->type == TOKEN_KEYWORD && token->keyword == KEYWORD_use)
	{
		readToken (token);
		skipOverParens (token);
	}

	/* PHP7 return type declaration or if parsing Zephir, skip function return
	 * type hint */
	if ((getInputLanguage () == Lang_php && token->type == TOKEN_COLON) ||
	    (getInputLanguage () == Lang_zephir && token->type == TOKEN_OPERATOR))
	{
		do
			readToken (token);
		while (token->type == TOKEN_IDENTIFIER ||
		       token->type == TOKEN_BACKSLASH);
	}

	if (token->type == TOKEN_OPEN_CURLY)
		enterScope (token, name->string, K_FUNCTION);
	else
		readNext = FALSE;

	if (nameFree)
		deleteToken (nameFree);

	return readNext;
}

/* parses declarations of the form
 * 	const NAME = VALUE */
static boolean parseConstant (tokenInfo *const token)
{
	tokenInfo *name;

	readToken (token); /* skip const keyword */
	if (token->type != TOKEN_IDENTIFIER && token->type != TOKEN_KEYWORD)
		return FALSE;

	name = newToken ();
	copyToken (name, token, TRUE);

	readToken (token);
	if (token->type == TOKEN_EQUAL_SIGN)
		makeSimplePhpTag (name, K_DEFINE, ACCESS_UNDEFINED);

	deleteToken (name);

	return token->type == TOKEN_EQUAL_SIGN;
}

/* parses declarations of the form
 * 	define('NAME', 'VALUE')
 * 	define(NAME, 'VALUE) */
static boolean parseDefine (tokenInfo *const token)
{
	int depth = 1;

	readToken (token); /* skip "define" identifier */
	if (token->type != TOKEN_OPEN_PAREN)
		return FALSE;

	readToken (token);
	if (token->type == TOKEN_STRING ||
		token->type == TOKEN_IDENTIFIER)
	{
		makeSimplePhpTag (token, K_DEFINE, ACCESS_UNDEFINED);
		readToken (token);
	}

	/* skip until the close parenthesis.
	 * no need to handle nested blocks since they would be invalid
	 * in this context anyway (the VALUE may only be a scalar, like
	 * 	42
	 * 	(42)
	 * and alike) */
	while (token->type != TOKEN_EOF && depth > 0)
	{
		switch (token->type)
		{
			case TOKEN_OPEN_PAREN:	depth++; break;
			case TOKEN_CLOSE_PAREN:	depth--; break;
			default: break;
		}
		readToken (token);
	}

	return FALSE;
}

static void readQualifiedName (tokenInfo *const token, vString *name,
                               tokenInfo *const lastToken)
{
	while (token->type == TOKEN_IDENTIFIER || token->type == TOKEN_BACKSLASH)
	{
		if (token->type == TOKEN_BACKSLASH)
			vStringPut (name, '\\');
		else
			vStringCat (name, token->string);
		copyToken (lastToken, token, TRUE);
		readToken (token);
	}
}

/* parses declarations of the form
 * 	use Foo
 * 	use Foo\Bar\Class
 * 	use Foo\Bar\Class as FooBarClass
 * 	use function Foo\Bar\func
 * 	use function Foo\Bar\func as foobarfunc
 * 	use const Foo\Bar\CONST
 * 	use const Foo\Bar\CONST as FOOBARCONST
 * 	use Foo, Bar
 * 	use Foo, Bar as Baz
 * 	use Foo as Test, Bar as Baz
 * 	use Foo\{Bar, Baz as Child, Nested\Other, Even\More as Something} */
static boolean parseUse (tokenInfo *const token)
{
	boolean readNext = FALSE;
	/* we can't know the use type, because class, interface and namespaces
	 * aliases are the same, and the only difference is the referenced name's
	 * type */
	const char *refType = "unknown";
	vString *refName = vStringNew ();
	tokenInfo *nameToken = newToken ();
	boolean grouped = FALSE;

	readToken (token); /* skip use keyword itself */
	if (token->type == TOKEN_KEYWORD && (token->keyword == KEYWORD_function ||
	                                     token->keyword == KEYWORD_const))
	{
		switch (token->keyword)
		{
			case KEYWORD_function:	refType = PhpKinds[K_FUNCTION].name;	break;
			case KEYWORD_const:		refType = PhpKinds[K_DEFINE].name;		break;
			default: break; /* silence compilers */
		}
		readNext = TRUE;
	}

	if (readNext)
		readToken (token);

	readQualifiedName (token, refName, nameToken);
	grouped = readNext = (token->type == TOKEN_OPEN_CURLY);

	do
	{
		size_t refNamePrefixLength = grouped ? vStringLength (refName) : 0;

		/* if it's either not the first name in a comma-separated list, or we
		 * are in a grouped alias and need to read the leaf name */
		if (readNext)
		{
			readToken (token);
			readQualifiedName (token, refName, nameToken);
		}

		if (token->type == TOKEN_KEYWORD && token->keyword == KEYWORD_as)
		{
			readToken (token);
			copyToken (nameToken, token, TRUE);
			readToken (token);
		}

		if (nameToken->type == TOKEN_IDENTIFIER && PhpKinds[K_ALIAS].enabled)
		{
			tagEntryInfo entry;

			initPhpEntry (&entry, nameToken, K_ALIAS, ACCESS_UNDEFINED);

			entry.extensionFields.typeRef[0] = refType;
			entry.extensionFields.typeRef[1] = vStringValue (refName);

			makePhpTagEntry (&entry);
		}

		vStringTruncate (refName, refNamePrefixLength);

		readNext = TRUE;
	}
	while (token->type == TOKEN_COMMA);

	if (grouped && token->type == TOKEN_CLOSE_CURLY)
		readToken (token);

	vStringDelete (refName);
	deleteToken (nameToken);

	return (token->type == TOKEN_SEMICOLON);
}

/* parses declarations of the form
 * 	$var = VALUE
 * 	$var; */
static boolean parseVariable (tokenInfo *const token)
{
	tokenInfo *name;
	boolean readNext = TRUE;
	accessType access = CurrentStatement.access;

	name = newToken ();
	copyToken (name, token, TRUE);

	readToken (token);
	if (token->type == TOKEN_EQUAL_SIGN)
	{
		phpKind kind = K_VARIABLE;

		if (token->parentKind == K_FUNCTION)
			kind = K_LOCAL_VARIABLE;

		readToken (token);
		if (token->type == TOKEN_KEYWORD &&
			token->keyword == KEYWORD_function &&
			PhpKinds[kind].enabled)
		{
			if (parseFunction (token, name))
				readToken (token);
			readNext = (boolean) (token->type == TOKEN_SEMICOLON);
		}
		else
		{
			makeSimplePhpTag (name, kind, access);
			readNext = FALSE;
		}
	}
	else if (token->type == TOKEN_SEMICOLON)
	{
		/* generate tags for variable declarations in classes
		 * 	class Foo {
		 * 		protected $foo;
		 * 	}
		 * but don't get fooled by stuff like $foo = $bar; */
		if (token->parentKind == K_CLASS ||
		    token->parentKind == K_INTERFACE ||
		    token->parentKind == K_TRAIT)
			makeSimplePhpTag (name, K_VARIABLE, access);
	}
	else
		readNext = FALSE;

	deleteToken (name);

	return readNext;
}

/* parses namespace declarations
 * 	namespace Foo {}
 * 	namespace Foo\Bar {}
 * 	namespace Foo;
 * 	namespace Foo\Bar;
 * 	namespace;
 * 	napespace {} */
static boolean parseNamespace (tokenInfo *const token)
{
	tokenInfo *nsToken = newToken ();

	vStringClear (CurrentNamesapce);
	copyToken (nsToken, token, FALSE);

	do
	{
		readToken (token);
		if (token->type == TOKEN_IDENTIFIER)
		{
			if (vStringLength (CurrentNamesapce) > 0)
			{
				const char *sep;

				sep = phpScopeSeparatorFor(K_NAMESPACE,
							   K_NAMESPACE);
				vStringCatS (CurrentNamesapce, sep);
			}
			vStringCat (CurrentNamesapce, token->string);
		}
	}
	while (token->type != TOKEN_EOF &&
		   token->type != TOKEN_SEMICOLON &&
		   token->type != TOKEN_OPEN_CURLY);

	vStringTerminate (CurrentNamesapce);
	if (vStringLength (CurrentNamesapce) > 0)
		makeNamespacePhpTag (nsToken, CurrentNamesapce);

	if (token->type == TOKEN_OPEN_CURLY)
		enterScope (token, NULL, -1);

	deleteToken (nsToken);

	return TRUE;
}

static void enterScope (tokenInfo *const parentToken,
						const vString *const extraScope,
						const int parentKind)
{
	tokenInfo *token = newToken ();
	int origParentKind = parentToken->parentKind;

	copyToken (token, parentToken, TRUE);

	if (extraScope)
	{
		token->parentKind = parentKind;
		addToScope (token, extraScope, origParentKind);
	}

	readToken (token);
	while (token->type != TOKEN_EOF &&
		   token->type != TOKEN_CLOSE_CURLY)
	{
		boolean readNext = TRUE;

		switch (token->type)
		{
			case TOKEN_OPEN_CURLY:
				enterScope (token, NULL, -1);
				break;

			case TOKEN_KEYWORD:
				switch (token->keyword)
				{
					/* handle anonymous classes */
					case KEYWORD_new:
						readToken (token);
						if (token->keyword != KEYWORD_class)
							readNext = FALSE;
						else
						{
							char buf[32];
							tokenInfo *name = newToken ();

							copyToken (name, token, TRUE);
							snprintf (buf, sizeof buf, "AnonymousClass%u", ++AnonymousID);
							vStringCopyS (name->string, buf);
							readNext = parseClassOrIface (token, K_CLASS, name);
							deleteToken (name);
						}
						break;

					case KEYWORD_class:		readNext = parseClassOrIface (token, K_CLASS, NULL);		break;
					case KEYWORD_interface:	readNext = parseClassOrIface (token, K_INTERFACE, NULL);	break;
					case KEYWORD_trait:		readNext = parseTrait (token);								break;
					case KEYWORD_function:	readNext = parseFunction (token, NULL);						break;
					case KEYWORD_const:		readNext = parseConstant (token);							break;
					case KEYWORD_define:	readNext = parseDefine (token);								break;

					case KEYWORD_use:
						/* aliases are only allowed at root scope, but the keyword
						 * is also used to i.e. "import" traits into a class */
						if (vStringLength (token->scope) == 0)
							readNext = parseUse (token);
						break;

					case KEYWORD_namespace:	readNext = parseNamespace (token);	break;

					case KEYWORD_private:	CurrentStatement.access = ACCESS_PRIVATE;	break;
					case KEYWORD_protected:	CurrentStatement.access = ACCESS_PROTECTED;	break;
					case KEYWORD_public:	CurrentStatement.access = ACCESS_PUBLIC;	break;
					case KEYWORD_var:		CurrentStatement.access = ACCESS_PUBLIC;	break;

					case KEYWORD_abstract:	CurrentStatement.impl = IMPL_ABSTRACT;		break;

					default: break;
				}
				break;

			case TOKEN_VARIABLE:
				readNext = parseVariable (token);
				break;

			default: break;
		}

		if (readNext)
			readToken (token);
	}

	copyToken (parentToken, token, FALSE);
	parentToken->parentKind = origParentKind;
	deleteToken (token);
}

static void findTags (boolean startsInPhpMode)
{
	tokenInfo *const token = newToken ();

	InPhp = startsInPhpMode;
	CurrentStatement.access = ACCESS_UNDEFINED;
	CurrentStatement.impl = IMPL_UNDEFINED;
	CurrentNamesapce = vStringNew ();
	FullScope = vStringNew ();
	AnonymousID = 0;

	do
	{
		enterScope (token, NULL, -1);
	}
	while (token->type != TOKEN_EOF); /* keep going even with unmatched braces */

	vStringDelete (FullScope);
	vStringDelete (CurrentNamesapce);
	deleteToken (token);
}

static void findPhpTags (void)
{
	findTags (FALSE);
}

static void findZephirTags (void)
{
	findTags (TRUE);
}

static void initializePhpParser (const langType language)
{
	Lang_php = language;
}

static void initializeZephirParser (const langType language)
{
	Lang_zephir = language;
}

extern parserDefinition* PhpParser (void)
{
	static const char *const extensions [] = { "php", "php3", "php4", "php5", "php7", "phtml", NULL };
	parserDefinition* def = parserNew ("PHP");
	def->kinds      = PhpKinds;
	def->kindCount  = ARRAY_SIZE (PhpKinds);
	def->extensions = extensions;
	def->parser     = findPhpTags;
	def->initialize = initializePhpParser;
	def->keywordTable = PhpKeywordTable;
	def->keywordCount = ARRAY_SIZE (PhpKeywordTable);
	return def;
}

extern parserDefinition* ZephirParser (void)
{
	static const char *const extensions [] = { "zep", NULL };
	parserDefinition* def = parserNew ("Zephir");
	def->kinds      = PhpKinds;
	def->kindCount  = ARRAY_SIZE (PhpKinds);
	def->extensions = extensions;
	def->parser     = findZephirTags;
	def->initialize = initializeZephirParser;
	def->keywordTable = PhpKeywordTable;
	def->keywordCount = ARRAY_SIZE (PhpKeywordTable);
	return def;
}
