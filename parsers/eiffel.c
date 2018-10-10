/*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Eiffel language
*   files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <limits.h>
#include <ctype.h>  /* to define tolower () */

#include "debug.h"
#include "keyword.h"
#include "routines.h"
#include "vstring.h"
#include "entry.h"
#include "parse.h"
#include "read.h"
#include "xtag.h"

/*
*   MACROS
*/
#define isident(c)            (isalnum(c) || (c) == '_')
#define isFreeOperatorChar(c) ((c) == '@' || (c) == '#' || \
                               (c) == '|' || (c) == '&')
#define isType(token,t)       (bool) ((token)->type == (t))
#define isKeyword(token,k)    (bool) ((token)->keyword == (k))

/*
*   DATA DECLARATIONS
*/

/*  Used to specify type of keyword.
 */
enum eKeywordId {
	KEYWORD_across,
	KEYWORD_alias,
	KEYWORD_all,
	KEYWORD_and,
	KEYWORD_as,
	KEYWORD_assign,
	KEYWORD_attached,
	KEYWORD_attribute,
	KEYWORD_check,
	KEYWORD_class,
	KEYWORD_convert,
	KEYWORD_create,
	KEYWORD_creation,
	KEYWORD_Current,
	KEYWORD_debug,
	KEYWORD_deferred,
	KEYWORD_detachable,
	KEYWORD_do,
	KEYWORD_else,
	KEYWORD_elseif,
	KEYWORD_end,
	KEYWORD_ensure,
	KEYWORD_expanded,
	KEYWORD_export,
	KEYWORD_external,
	KEYWORD_false,
	KEYWORD_feature,
	KEYWORD_from,
	KEYWORD_frozen,
	KEYWORD_if,
	KEYWORD_implies,
	KEYWORD_infix,
	KEYWORD_inherit,
	KEYWORD_inspect,
	KEYWORD_invariant,
	KEYWORD_is,
	KEYWORD_like,
	KEYWORD_local,
	KEYWORD_loop,
	KEYWORD_not,
	KEYWORD_note,
	KEYWORD_obsolete,
	KEYWORD_old,
	KEYWORD_once,
	KEYWORD_or,
	KEYWORD_prefix,
	KEYWORD_redefine,
	KEYWORD_rename,
	KEYWORD_require,
	KEYWORD_rescue,
	KEYWORD_Result,
	KEYWORD_retry,
	KEYWORD_select,
	KEYWORD_separate,
	KEYWORD_strip,
	KEYWORD_then,
	KEYWORD_true,
	KEYWORD_undefine,
	KEYWORD_unique,
	KEYWORD_until,
	KEYWORD_variant,
	KEYWORD_when,
	KEYWORD_xor
};
typedef int keywordId; /* to allow KEYWORD_NONE */

typedef enum eTokenType {
	TOKEN_EOF,
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
	TOKEN_QUESTION,
	TOKEN_SEMICOLON,
	TOKEN_STRING,
	TOKEN_TILDE
} tokenType;

typedef struct sTokenInfo {
	tokenType type;
	keywordId keyword;
	bool   isExported;
	vString*  string;
	vString*  className;
	vString*  featureName;
} tokenInfo;

/*
*   DATA DEFINITIONS
*/

static langType Lang_eiffel;

typedef enum {
	EKIND_CLASS, EKIND_FEATURE, EKIND_LOCAL, EKIND_QUALIFIED_TAGS
} eiffelKind;

static kindDefinition EiffelKinds [] = {
	{ true,  'c', "class",   "classes"},
	{ true,  'f', "feature", "features"},
	{ false, 'l', "local",   "local entities"}
};

static const keywordTable EiffelKeywordTable [] = {
	/* keyword          keyword ID */
	{ "across",         KEYWORD_across     },
	{ "alias",          KEYWORD_alias      },
	{ "all",            KEYWORD_all        },
	{ "and",            KEYWORD_and        },
	{ "as",             KEYWORD_as         },
	{ "assign",         KEYWORD_assign     },
	{ "attached",       KEYWORD_attached   },
	{ "attribute",      KEYWORD_attribute  },
	{ "check",          KEYWORD_check      },
	{ "class",          KEYWORD_class      },
	{ "convert",        KEYWORD_convert    },
	{ "create",         KEYWORD_create     },
	{ "creation",       KEYWORD_creation   },
	{ "current",        KEYWORD_Current    },
	{ "debug",          KEYWORD_debug      },
	{ "deferred",       KEYWORD_deferred   },
	{ "detachable",     KEYWORD_detachable },
	{ "do",             KEYWORD_do         },
	{ "else",           KEYWORD_else       },
	{ "elseif",         KEYWORD_elseif     },
	{ "end",            KEYWORD_end        },
	{ "ensure",         KEYWORD_ensure     },
	{ "expanded",       KEYWORD_expanded   },
	{ "export",         KEYWORD_export     },
	{ "external",       KEYWORD_external   },
	{ "false",          KEYWORD_false      },
	{ "feature",        KEYWORD_feature    },
	{ "from",           KEYWORD_from       },
	{ "frozen",         KEYWORD_frozen     },
	{ "if",             KEYWORD_if         },
	{ "implies",        KEYWORD_implies    },
	{ "indexing",       KEYWORD_note       },
	{ "infix",          KEYWORD_infix      },
	{ "inherit",        KEYWORD_inherit    },
	{ "insert",         KEYWORD_inherit    },
	{ "inspect",        KEYWORD_inspect    },
	{ "invariant",      KEYWORD_invariant  },
	{ "is",             KEYWORD_is         },
	{ "like",           KEYWORD_like       },
	{ "local",          KEYWORD_local      },
	{ "loop",           KEYWORD_loop       },
	{ "not",            KEYWORD_not        },
	{ "note",           KEYWORD_note       },
	{ "obsolete",       KEYWORD_obsolete   },
	{ "old",            KEYWORD_old        },
	{ "once",           KEYWORD_once       },
	{ "or",             KEYWORD_or         },
	{ "prefix",         KEYWORD_prefix     },
	{ "redefine",       KEYWORD_redefine   },
	{ "rename",         KEYWORD_rename     },
	{ "require",        KEYWORD_require    },
	{ "rescue",         KEYWORD_rescue     },
	{ "result",         KEYWORD_Result     },
	{ "retry",          KEYWORD_retry      },
	{ "select",         KEYWORD_select     },
	{ "separate",       KEYWORD_separate   },
	{ "strip",          KEYWORD_strip      },
	{ "then",           KEYWORD_then       },
	{ "true",           KEYWORD_true       },
	{ "undefine",       KEYWORD_undefine   },
	{ "unique",         KEYWORD_unique     },
	{ "until",          KEYWORD_until      },
	{ "variant",        KEYWORD_variant    },
	{ "when",           KEYWORD_when       },
	{ "xor",            KEYWORD_xor        }
};

/*
*   FUNCTION DEFINITIONS
*/

/*
*   Tag generation functions
*/

static void makeEiffelClassTag (tokenInfo *const token)
{
	if (EiffelKinds [EKIND_CLASS].enabled)
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;

		initTagEntry (&e, name, EKIND_CLASS);

		makeTagEntry (&e);
	}
	vStringCopy (token->className, token->string);
}

static void makeEiffelFeatureTag (tokenInfo *const token)
{
	if (EiffelKinds [EKIND_FEATURE].enabled  &&
		(token->isExported  ||  isXtagEnabled(XTAG_FILE_SCOPE)))
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;

		initTagEntry (&e, name, EKIND_FEATURE);

		e.isFileScope = (bool) (! token->isExported);
		if (e.isFileScope)
			markTagExtraBit (&e, XTAG_FILE_SCOPE);
		e.extensionFields.scopeKindIndex = EKIND_CLASS;
		e.extensionFields.scopeName = vStringValue (token->className);

		makeTagEntry (&e);

		if (isXtagEnabled(XTAG_QUALIFIED_TAGS))
		{
			vString* qualified = vStringNewInit (vStringValue (token->className));
			vStringPut (qualified, '.');
			vStringCat (qualified, token->string);
			e.name = vStringValue (qualified);
			markTagExtraBit (&e, XTAG_QUALIFIED_TAGS);
			makeTagEntry (&e);
			vStringDelete (qualified);
		}
	}
	vStringCopy (token->featureName, token->string);
}

static void makeEiffelLocalTag (tokenInfo *const token)
{
	if (EiffelKinds [EKIND_LOCAL].enabled && isXtagEnabled(XTAG_FILE_SCOPE))
	{
		const char *const name = vStringValue (token->string);
		vString* scope = vStringNew ();
		tagEntryInfo e;

		initTagEntry (&e, name, EKIND_LOCAL);

		e.isFileScope = true;
		markTagExtraBit (&e, XTAG_FILE_SCOPE);

		vStringCopy (scope, token->className);
		vStringPut (scope, '.');
		vStringCat (scope, token->featureName);

		e.extensionFields.scopeKindIndex = EKIND_FEATURE;
		e.extensionFields.scopeName = vStringValue (scope);

		makeTagEntry (&e);
		vStringDelete (scope);
	}
}

/*
*   Parsing functions
*/

static int skipToCharacter (const int c)
{
	int d;

	do
	{
		d = getcFromInputFile ();
	} while (d != EOF  &&  d != c);

	return d;
}

/*  If a numeric is passed in 'c', this is used as the first digit of the
 *  numeric being parsed.
 */
static vString *parseInteger (int c)
{
	vString *string = vStringNew ();

	if (c == '\0')
		c = getcFromInputFile ();
	if (c == '-')
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	}
	else if (! isdigit (c))
		c = getcFromInputFile ();
	while (c != EOF  &&  (isdigit (c)  ||  c == '_'))
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	}
	ungetcToInputFile (c);

	return string;
}

static vString *parseNumeric (int c)
{
	vString *string = vStringNew ();
	vString *integer = parseInteger (c);
	vStringCopy (string, integer);
	vStringDelete (integer);

	c = getcFromInputFile ();
	if (c == '.')
	{
		integer = parseInteger ('\0');
		vStringPut (string, c);
		vStringCat (string, integer);
		vStringDelete (integer);
		c = getcFromInputFile ();
	}
	if (tolower (c) == 'e')
	{
		integer = parseInteger ('\0');
		vStringPut (string, c);
		vStringCat (string, integer);
		vStringDelete (integer);
	}
	else if (!isspace (c))
		ungetcToInputFile (c);

	return string;
}

static int parseEscapedCharacter (void)
{
	int d = '\0';
	int c = getcFromInputFile ();

	switch (c)
	{
		case 'A':  d = '@';   break;
		case 'B':  d = '\b';  break;
		case 'C':  d = '^';   break;
		case 'D':  d = '$';   break;
		case 'F':  d = '\f';  break;
		case 'H':  d = '\\';  break;
		case 'L':  d = '~';   break;
		case 'N':  d = '\n';  break;
		case 'Q':  d = '`';   break;
		case 'R':  d = '\r';  break;
		case 'S':  d = '#';   break;
		case 'T':  d = '\t';  break;
		case 'U':  d = '\0';  break;
		case 'V':  d = '|';   break;
		case '%':  d = '%';   break;
		case '\'': d = '\'';  break;
		case '"':  d = '"';   break;
		case '(':  d = '[';   break;
		case ')':  d = ']';   break;
		case '<':  d = '{';   break;
		case '>':  d = '}';   break;

		case '\n': skipToCharacter ('%'); break;

		case '/':
		{
			vString *string = parseInteger ('\0');
			const char *value = vStringValue (string);
			const unsigned long ascii = atol (value);
			vStringDelete (string);

			c = getcFromInputFile ();
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
	int c = getcFromInputFile ();
	int result = c;

	if (c == '%')
		result = parseEscapedCharacter ();

	c = getcFromInputFile ();
	if (c != '\'')
		skipToCharacter ('\n');

	return result;
}

static void parseString (vString *const string)
{
	bool verbatim = false;
	bool align = false;
	bool end = false;
	vString *verbatimCloser = vStringNew ();
	vString *lastLine = vStringNew ();
	int prev = '\0';
	int c;

	while (! end)
	{
		c = getcFromInputFile ();
		if (c == EOF)
			end = true;
		else if (c == '"')
		{
			if (! verbatim)
				end = true;
			else
				end = (bool) (strcmp (vStringValue (lastLine),
				                         vStringValue (verbatimCloser)) == 0);
		}
		else if (c == '\n')
		{
			if (verbatim)
				vStringClear (lastLine);
			if (prev == '[' /* ||  prev == '{' */)
			{
				verbatim = true;
				vStringClear (verbatimCloser);
				vStringClear (lastLine);
				if (prev == '{')
					vStringPut (verbatimCloser, '}');
				else
				{
					vStringPut (verbatimCloser, ']');
					align = true;
				}
				vStringNCat (verbatimCloser, string, vStringLength (string) - 1);
				vStringClear (string);
			}
			if (verbatim && align)
			{
				do
					c = getcFromInputFile ();
				while (isspace (c));
			}
		}
		else if (c == '%')
			c = parseEscapedCharacter ();
		if (! end)
		{
			vStringPut (string, c);
			if (verbatim)
				vStringPut (lastLine, c);
			prev = c;
		}
	}
	vStringDelete (lastLine);
	vStringDelete (verbatimCloser);
}

/*  Read a C identifier beginning with "firstChar" and places it into "name".
 */
static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;

	do
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	} while (isident (c));

	if (!isspace (c))
		ungetcToInputFile (c);  /* unget non-identifier character */
}

static void parseFreeOperator (vString *const string, const int firstChar)
{
	int c = firstChar;

	do
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	} while (c > ' ');

	if (!isspace (c))
		ungetcToInputFile (c);  /* unget non-identifier character */
}

static void copyToken (tokenInfo* dst, const tokenInfo *src)
{
	dst->type       = src->type;
	dst->keyword    = src->keyword;
	dst->isExported = src->isExported;

	vStringCopy (dst->string, src->string);
	vStringCopy (dst->className, src->className);
	vStringCopy (dst->featureName, src->featureName);
}

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->isExported	= true;

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

static void readToken (tokenInfo *const token)
{
	int c;

	token->type    = TOKEN_UNDEFINED;
	token->keyword = KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:

	do
		c = getcFromInputFile ();
	while (c == '\t'  ||  c == ' '  ||  c == '\n');

	switch (c)
	{
		case EOF:  token->type = TOKEN_EOF;                break;
		case ';':  token->type = TOKEN_SEMICOLON;          break;
		case '!':  token->type = TOKEN_BANG;               break;
		case '}':  token->type = TOKEN_CLOSE_BRACE;        break;
		case ']':  token->type = TOKEN_CLOSE_BRACKET;      break;
		case ')':  token->type = TOKEN_CLOSE_PAREN;        break;
		case ',':  token->type = TOKEN_COMMA;              break;
		case '$':  token->type = TOKEN_DOLLAR;             break;
		case '.':  token->type = TOKEN_DOT;                break;
		case '{':  token->type = TOKEN_OPEN_BRACE;         break;
		case '[':  token->type = TOKEN_OPEN_BRACKET;       break;
		case '(':  token->type = TOKEN_OPEN_PAREN;         break;
		case '~':  token->type = TOKEN_TILDE;              break;


		case '+':
		case '*':
		case '^':
		case '=':  token->type = TOKEN_OPERATOR;           break;

		case '-':
			c = getcFromInputFile ();
			if (c == '>')
				token->type = TOKEN_CONSTRAINT;
			else if (c == '-')  /* is this the start of a comment? */
			{
				skipToCharacter ('\n');
				goto getNextChar;
			}
			else
			{
				if (!isspace (c))
					ungetcToInputFile (c);
				token->type = TOKEN_OPERATOR;
			}
			break;

		case '?':
		case ':':
		{
			int c2 = getcFromInputFile ();
			if (c2 == '=')
				token->type = TOKEN_OPERATOR;
			else
			{
				if (!isspace (c2))
					ungetcToInputFile (c2);
				if (c == ':')
					token->type = TOKEN_COLON;
				else
					token->type = TOKEN_QUESTION;
			}
			break;
		}

		case '<':
			c = getcFromInputFile ();
			if (c != '='  &&  c != '>'  &&  !isspace (c))
				ungetcToInputFile (c);
			token->type = TOKEN_OPERATOR;
			break;

		case '>':
			c = getcFromInputFile ();
			if (c != '='  &&  c != '>'  &&  !isspace (c))
				ungetcToInputFile (c);
			token->type = TOKEN_OPERATOR;
			break;

		case '/':
			c = getcFromInputFile ();
			if (c != '/'  &&  c != '='  &&  !isspace (c))
				ungetcToInputFile (c);
			token->type = TOKEN_OPERATOR;
			break;

		case '\\':
			c = getcFromInputFile ();
			if (c != '\\'  &&  !isspace (c))
				ungetcToInputFile (c);
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
				token->keyword = lookupCaseKeyword (vStringValue (token->string), Lang_eiffel);
				if (isKeyword (token, KEYWORD_NONE))
					token->type = TOKEN_IDENTIFIER;
				else
					token->type = TOKEN_KEYWORD;
			}
			else if (isdigit (c))
			{
				vString* numeric = parseNumeric (c);
				vStringCat (token->string, numeric);
				vStringDelete (numeric);
				token->type = TOKEN_NUMERIC;
			}
			else if (isFreeOperatorChar (c))
			{
				parseFreeOperator (token->string, c);
				token->type = TOKEN_OPERATOR;
			}
			else
				token->type = TOKEN_UNDEFINED;
			break;
	}
}

/*
*   Scanning functions
*/

static bool isIdentifierMatch (
		const tokenInfo *const token, const char *const name)
{
	return (bool) (isType (token, TOKEN_IDENTIFIER)  &&
		strcasecmp (vStringValue (token->string), name) == 0);
}

static bool findToken (tokenInfo *const token, const tokenType type)
{
	while (! isType (token, type) && ! isType (token, TOKEN_EOF))
		readToken (token);
	return isType (token, type);
}

static bool findKeyword (tokenInfo *const token, const keywordId keyword)
{
	while (! isKeyword (token, keyword) && ! isType (token, TOKEN_EOF))
		readToken (token);
	return isKeyword (token, keyword);
}

static bool parseType (tokenInfo *const token);

static void parseGeneric (tokenInfo *const token, bool declaration CTAGS_ATTR_UNUSED)
{
	unsigned int depth = 0;

	Assert (isType (token, TOKEN_OPEN_BRACKET));
	do
	{
		if (isType (token, TOKEN_OPEN_BRACKET))
		{
			++depth;
			readToken (token);
		}
		else if (isType (token, TOKEN_CLOSE_BRACKET))
		{
			--depth;
			readToken (token);
		}
		else
			parseType (token);
	} while (depth > 0 && ! isType (token, TOKEN_EOF));
}

static bool parseType (tokenInfo *const token)
{
	tokenInfo* const id = newToken ();
	copyToken (id, token);
	readToken (token);
	if (isType (token, TOKEN_COLON))  /* check for "{entity: TYPE}" */
	{
		readToken (id);
		readToken (token);
	}
	if (isKeyword (id, KEYWORD_like))
	{
		if (isType (token, TOKEN_IDENTIFIER) ||
				isKeyword (token, KEYWORD_Current))
			readToken (token);
	}
	else
	{
		if (isKeyword (id, KEYWORD_attached) ||
		    isKeyword (id, KEYWORD_detachable) ||
		    isKeyword (id, KEYWORD_expanded))
		{
			copyToken (id, token);
			readToken (token);
		}
		if (isType (id, TOKEN_IDENTIFIER))
		{
			if (isType (token, TOKEN_OPEN_BRACKET))
				parseGeneric (token, false);
			else if ((strcmp ("BIT", vStringValue (id->string)) == 0))
				readToken (token);  /* read token after number of bits */
		}
	}
	deleteToken (id);
	return true;
}

static void parseEntityType (tokenInfo *const token)
{
	Assert (isType (token, TOKEN_COLON));
	readToken (token);

	if (isType (token, TOKEN_BANG) || isType (token, TOKEN_QUESTION))
		readToken (token);  /* skip over '!' or '?' */
	parseType (token);
}

static void parseLocal (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_local));
	readToken (token);

	/*  Check keyword first in case local clause is empty
	 */
	while (! isKeyword (token, KEYWORD_do)  &&
		   ! isKeyword (token, KEYWORD_once) &&
		   ! isKeyword (token, KEYWORD_attribute) &&
		   ! isType (token, TOKEN_EOF))
	{
		if (isType (token, TOKEN_IDENTIFIER))
			makeEiffelLocalTag (token);
		readToken (token);
		if (isType (token, TOKEN_COLON))
			parseEntityType (token);
	}
}

static void findFeatureEnd (tokenInfo *const token)
{
	bool isFound = isKeyword (token, KEYWORD_is);
	if (isFound)
		readToken (token);
	switch (token->keyword)
	{
		case KEYWORD_attribute:
		case KEYWORD_deferred:
		case KEYWORD_do:
		case KEYWORD_external:
		case KEYWORD_local:
		case KEYWORD_note:
		case KEYWORD_obsolete:
		case KEYWORD_once:
		case KEYWORD_require:
		{
			int depth = 1;

			while (depth > 0 && ! isType (token, TOKEN_EOF))
			{
				switch (token->keyword)
				{
					case KEYWORD_check:
					case KEYWORD_debug:
					case KEYWORD_from:
					case KEYWORD_across:
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

		default:
			/* is this a manifest constant? */
			if (isFound || isType (token, TOKEN_OPERATOR)) {
				if (isType (token, TOKEN_OPERATOR))
					readToken (token);
				readToken (token);
			}
			break;
	}
}

static bool readFeatureName (tokenInfo *const token)
{
	bool isFeatureName = false;

	if (isKeyword (token, KEYWORD_frozen))
		readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
		isFeatureName = true;
	else if (isKeyword (token, KEYWORD_assign))  /* legacy code */
		isFeatureName = true;
	else if (isKeyword (token, KEYWORD_infix)  ||
			isKeyword (token, KEYWORD_prefix))
	{
		readToken (token);
		if (isType (token, TOKEN_STRING))
			isFeatureName = true;
	}
	return isFeatureName;
}

static void parseArguments (tokenInfo *const token)
{
	if (findToken (token, TOKEN_CLOSE_PAREN))
		readToken (token);
}

static bool parseFeature (tokenInfo *const token)
{
	bool found = false;
	while (readFeatureName (token))
	{
		found = true;
		makeEiffelFeatureTag (token);
		readToken (token);
		if (isType (token, TOKEN_COMMA))
			readToken (token);
	}
	if (found)
	{
		if (isKeyword (token, KEYWORD_alias)) {
			readToken (token);
			if (isType (token, TOKEN_STRING))
				makeEiffelFeatureTag (token);
			readToken (token);
		}
		if (isType (token, TOKEN_OPEN_PAREN))  /* arguments? */
			parseArguments (token);
		if (isType (token, TOKEN_COLON))       /* a query? */
			parseEntityType (token);
		if (isKeyword (token, KEYWORD_assign))
		{
			readToken (token);
			readToken (token);
		}
		if (isKeyword (token, KEYWORD_obsolete))
		{
			readToken (token);
			if (isType (token, TOKEN_STRING))
				readToken (token);
		}
		findFeatureEnd (token);
	}
	return found;
}

static void parseExport (tokenInfo *const token)
{
	token->isExported = true;
	readToken (token);
	if (isType (token, TOKEN_OPEN_BRACE))
	{
		token->isExported = false;
		while (! isType (token, TOKEN_CLOSE_BRACE) &&
		       ! isType (token, TOKEN_EOF))
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
		if (! isKeyword (token, KEYWORD_feature) &&
			! isKeyword (token, KEYWORD_invariant) &&
			! isKeyword (token, KEYWORD_note))
		{
			if (! parseFeature (token))
				readToken (token);
		}
	} while (! isKeyword (token, KEYWORD_end) &&
			 ! isKeyword (token, KEYWORD_invariant) &&
			 ! isKeyword (token, KEYWORD_note) &&
			 ! isType (token, TOKEN_EOF));
}

static void parseRename (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_rename));
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
					makeEiffelFeatureTag (token);  /* renamed feature */
					readToken (token);
				}
			}
		}
	} while (isType (token, TOKEN_COMMA));
}

static void parseInherit (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_inherit));
	readToken (token);
	while (isType (token, TOKEN_IDENTIFIER))
	{
		parseType (token);
		if (isType (token, TOKEN_KEYWORD))
		{
			switch (token->keyword)  /* check for feature adaptation */
			{
				case KEYWORD_rename:
					parseRename (token);
				case KEYWORD_export:
				case KEYWORD_undefine:
				case KEYWORD_redefine:
				case KEYWORD_select:
					if (findKeyword (token, KEYWORD_end))
						readToken (token);
					break;

				case KEYWORD_end:
					readToken (token);
					break;

				default: break;
			}
		}
		if (isType (token, TOKEN_SEMICOLON))
			readToken (token);
	}
}

static void parseConvert (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_convert));
	do
	{
		readToken (token);
		if (! isType (token, TOKEN_IDENTIFIER))
			break;
		else if (isType (token, TOKEN_OPEN_PAREN))
		{
			while (! isType (token, TOKEN_CLOSE_PAREN) &&
			       ! isType (token, TOKEN_EOF))
				readToken (token);
		}
		else if (isType (token, TOKEN_COLON))
		{
			readToken (token);
			if (! isType (token, TOKEN_OPEN_BRACE))
				break;
			else while (! isType (token, TOKEN_CLOSE_BRACE))
				readToken (token);
		}
	} while (isType (token, TOKEN_COMMA));
}

static void parseClass (tokenInfo *const token)
{
	Assert (isKeyword (token, KEYWORD_class));
	readToken (token);
	if (isType (token, TOKEN_IDENTIFIER))
	{
		makeEiffelClassTag (token);
		readToken (token);
	}

	do
	{
		if (isType (token, TOKEN_OPEN_BRACKET))
			parseGeneric (token, true);
		else if (! isType (token, TOKEN_KEYWORD))
			readToken (token);
		else switch (token->keyword)
		{
			case KEYWORD_inherit:  parseInherit (token);        break;
			case KEYWORD_feature:  parseFeatureClauses (token); break;
			case KEYWORD_convert:  parseConvert (token);        break;
			default:               readToken (token);           break;
		}
	} while (! isKeyword (token, KEYWORD_end) &&
	         ! isType (token, TOKEN_EOF));
}

static void initialize (const langType language)
{
	Lang_eiffel = language;
}

static void findEiffelTags (void)
{
	tokenInfo *const token = newToken ();

	while (findKeyword (token, KEYWORD_class))
		parseClass (token);
	deleteToken (token);
}

extern parserDefinition* EiffelParser (void)
{
	static const char *const extensions [] = { "e", NULL };
	parserDefinition* def = parserNew ("Eiffel");
	def->kindTable      = EiffelKinds;
	def->kindCount  = ARRAY_SIZE (EiffelKinds);
	def->extensions = extensions;
	def->parser     = findEiffelTags;
	def->initialize = initialize;
	def->keywordTable = EiffelKeywordTable;
	def->keywordCount = ARRAY_SIZE (EiffelKeywordTable);
	return def;
}
