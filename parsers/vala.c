/*
*   Copyright (c) 2019, Masatake Yamato <yamato@redhat.com>
*   Copyright (c) 2019, Alberto Fanjul <albertofanjul@gmail.com>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning Vala source files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"        /* must always come first */

#include "tokeninfo.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "keyword.h"
#include "entry.h"

#include <string.h>

/*
*   MACROS
*/

#define tokenEqType(TKN,T)     ((TKN)->type == T)


/*
*   DATA DEFINITIONS
*/

typedef enum {
	K_UNDEFINED = -1,
	K_CLASS,
	K_STRUCT,
	K_INTERFACE,
	K_ENUM,
	K_ENUMVALUE,
	K_ERRORDOMAIN,
	K_ERRORCODE,
	K_DELEGATE,
	K_SIGNAL,
	K_FIELD,
	K_METHOD,
	K_PROP,
	K_LOCAL,
	K_NAMESPACE,
	COUNT_KIND
} valaKind;

static kindDefinition ValaKinds [] = {
	{ true,  'c', "class",       "classes" },
	{ true,  's', "struct",      "structures" },
	{ true,  'i', "interface",   "interfaces" },
	{ true,  'e', "enum",        "enumerations" },
	{ true,  'v', "enumvalue",   "enumeration Values" },
	{ true,  'E', "errordomain", "error domains" },
	{ true,  'r', "errorcode",   "error codes" },
	{ true,  'd', "delegate",    "delegates" },
	{ true,  'S', "signal",      "signals" },
	{ true,  'f', "field",       "fields" },
	{ true,  'm', "method",      "methods" },
	{ true,  'p', "property",    "properties" },
	{ false, 'l', "local",       "local variables" },
	{ true,  'n', "namespace",   "namespace" },
};

enum eKeywordId
{
	KEYWORD_STRING,
	KEYWORD_INT,
	KEYWORD_DOUBLE,
	KEYWORD_FLOAT,
	KEYWORD_BOOL,
	KEYWORD_VOID,
	KEYWORD_TYPE,
	KEYWORD_ABSTRACT,
	KEYWORD_AS,
	KEYWORD_ASYNC,
	KEYWORD_BASE,
	KEYWORD_BREAK,
	KEYWORD_CASE,
	KEYWORD_CATCH,
	KEYWORD_CLASS,
	KEYWORD_CONST,
	KEYWORD_CONSTRUCT,
	KEYWORD_CONTINUE,
	KEYWORD_DEFAULT,
	KEYWORD_DELEGATE,
	KEYWORD_DELETE,
	KEYWORD_DO,
	KEYWORD_DYNAMIC,
	KEYWORD_ELSE,
	KEYWORD_ENSURES,
	KEYWORD_ENUM,
	KEYWORD_ERRORDOMAIN,
	KEYWORD_EXTERN,
	KEYWORD_FALSE,
	KEYWORD_FINALLY,
	KEYWORD_FOR,
	KEYWORD_FOREACH,
	KEYWORD_GET,
	KEYWORD_GLOBAL,
	KEYWORD_IF,
	KEYWORD_IN,
	KEYWORD_INLINE,
	KEYWORD_INTERFACE,
	KEYWORD_INTERNAL,
	KEYWORD_IS,
	KEYWORD_LOCK,
	KEYWORD_NAMESPACE,
	KEYWORD_NEW,
	KEYWORD_NULL,
	KEYWORD_OUT,
	KEYWORD_OVERRIDE,
	KEYWORD_OWNED,
	KEYWORD_PRIVATE,
	KEYWORD_PROTECTED,
	KEYWORD_PUBLIC,
	KEYWORD_REF,
	KEYWORD_REQUIRES,
	KEYWORD_RETURN,
	KEYWORD_SET,
	KEYWORD_SIGNAL,
	KEYWORD_SIZEOF,
	KEYWORD_STATIC,
	KEYWORD_STRUCT,
	KEYWORD_SWITCH,
	KEYWORD_THIS,
	KEYWORD_THROW,
	KEYWORD_THROWS,
	KEYWORD_TRUE,
	KEYWORD_TRY,
	KEYWORD_TYPEOF,
	KEYWORD_UNOWNED,
	KEYWORD_USING,
	KEYWORD_VALUE,
	KEYWORD_VAR,
	KEYWORD_VIRTUAL,
	KEYWORD_WEAK,
	KEYWORD_WHILE,
	KEYWORD_YIELD,

};

typedef int keywordId; /* to allow KEYWORD_NONE */

static const keywordTable ValaKeywordTable [] = {
	{ "string", KEYWORD_STRING },
	{ "int", KEYWORD_INT },
	{ "double", KEYWORD_DOUBLE },
	{ "float", KEYWORD_FLOAT },
	{ "bool", KEYWORD_BOOL },

	{ "void",   KEYWORD_VOID  },
	{ "Type",       KEYWORD_TYPE },
	{ "abstract",       KEYWORD_ABSTRACT },
	{ "as",       KEYWORD_AS },
	{ "async",       KEYWORD_ASYNC },
	{ "base",       KEYWORD_BASE },
	{ "break",       KEYWORD_BREAK },
	{ "case",       KEYWORD_CASE },
	{ "catch",       KEYWORD_CATCH },
	{ "class",       KEYWORD_CLASS },
	{ "const",       KEYWORD_CONST },
	{ "construct",       KEYWORD_CONSTRUCT },
	{ "continue",       KEYWORD_CONTINUE },
	{ "default",       KEYWORD_DEFAULT },
	{ "delegate",       KEYWORD_DELEGATE },
	{ "delete",       KEYWORD_DELETE },
	{ "do",       KEYWORD_DO },
	{ "dynamic",       KEYWORD_DYNAMIC },
	{ "else",       KEYWORD_ELSE },
	{ "ensures",       KEYWORD_ENSURES },
	{ "enum",       KEYWORD_ENUM },
	{ "errordomain",       KEYWORD_ERRORDOMAIN },
	{ "extern",       KEYWORD_EXTERN },
	{ "false",       KEYWORD_FALSE },
	{ "finally",       KEYWORD_FINALLY },
	{ "for",       KEYWORD_FOR },
	{ "foreach",       KEYWORD_FOREACH },
	{ "get",       KEYWORD_GET },
	{ "global",       KEYWORD_GLOBAL },
	{ "if",       KEYWORD_IF },
	{ "in",       KEYWORD_IN },
	{ "inline",       KEYWORD_INLINE },
	{ "interface",       KEYWORD_INTERFACE },
	{ "internal",       KEYWORD_INTERNAL },
	{ "is",       KEYWORD_IS },
	{ "lock",       KEYWORD_LOCK },
	{ "namespace",       KEYWORD_NAMESPACE },
	{ "new",       KEYWORD_NEW },
	{ "null",       KEYWORD_NULL },
	{ "out",       KEYWORD_OUT },
	{ "override",       KEYWORD_OVERRIDE },
	{ "owned",       KEYWORD_OWNED },
	{ "private",       KEYWORD_PRIVATE },
	{ "protected",       KEYWORD_PROTECTED },
	{ "public",       KEYWORD_PUBLIC },
	{ "ref",       KEYWORD_REF },
	{ "requires",       KEYWORD_REQUIRES },
	{ "return",       KEYWORD_RETURN },
	{ "set",       KEYWORD_SET },
	{ "signal",       KEYWORD_SIGNAL },
	{ "sizeof",       KEYWORD_SIZEOF },
	{ "static",       KEYWORD_STATIC },
	{ "struct",       KEYWORD_STRUCT },
	{ "switch",       KEYWORD_SWITCH },
	{ "this",       KEYWORD_THIS },
	{ "throw",       KEYWORD_THROW },
	{ "throws",       KEYWORD_THROWS },
	{ "true",       KEYWORD_TRUE },
	{ "try",       KEYWORD_TRY },
	{ "typeof",       KEYWORD_TYPEOF },
	{ "unowned",       KEYWORD_UNOWNED },
	{ "using",       KEYWORD_USING },
	{ "value",       KEYWORD_VALUE },
	{ "var",       KEYWORD_VAR },
	{ "virtual",       KEYWORD_VIRTUAL },
	{ "weak",       KEYWORD_WEAK },
	{ "while",       KEYWORD_WHILE },
	{ "yield",       KEYWORD_YIELD },
};

enum ValaTokenType {
	/* 0..255 are the byte's value */
	TOKEN_EOF = 256,
	TOKEN_UNDEFINED,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
	TOKEN_EOL,
	TOKEN_STRING,
};


/*
*   FUNCTION PROTOTYPES
*/

static void readToken (tokenInfo *const token, void *data);
static void parseNamespace (tokenInfo *const token, int corkIndex);
static void parseClass (tokenInfo *const token, int corkIndex);
static void parseStatement (tokenInfo *const token, int corkIndex);


/*
*   DATA DEFINITIONS
*/

static bool trianglePairState = true;
static struct tokenTypePair typePairs [] = {
	{ '{', '}', NULL },
	{ '[', ']', NULL },
	{ '(', ')', NULL },
	{ '<', '>', &trianglePairState },
};

static struct tokenInfoClass valaTokenInfoClass = {
	.nPreAlloc        = 4,
	.typeForUndefined = TOKEN_UNDEFINED,
	.keywordNone      = KEYWORD_NONE,
	.typeForKeyword   = TOKEN_KEYWORD,
	.typeForEOF       = TOKEN_EOF,
	.extraSpace       = 0,
	.pairs            = typePairs,
	.pairCount        = ARRAY_SIZE (typePairs),
	.init             = NULL,
	.read             = readToken,
	.clear            = NULL,
	.copy             = NULL,
};


/*
*   FUNCTION DEFINITIONS
*/

static bool tokenIsEmpty (tokenInfo *token)
{
	return vStringIsEmpty (token->string);
}

static keywordId resolveKeyword (vString *string)
{
	char *s = vStringValue (string);
	static langType lang = LANG_AUTO;

	if (lang == LANG_AUTO)
		lang = getInputLanguage ();

	return lookupKeyword (s, lang);
}

static void readString (tokenInfo *token)
{
	int c;
	while (1)
	{
		c = getcFromInputFile ();
		switch (c)
		{
		case EOF:
			return;
		case '"':
			tokenPutc (token, c);
			return;
		default:
			tokenPutc (token, c);
			break;
		}
	}
}

static void readIdentifier (tokenInfo *token)
{

	while (1)
	{
		int c = getcFromInputFile ();
		if (c == EOF)
			return;
		else if ((!tokenIsEmpty (token)) &&
				 (isalnum (c) || c == '_'))
			tokenPutc (token, c);
		else if (isalpha (c) || c == '_')
			tokenPutc (token, c);
		else
		{
			ungetcToInputFile (c);
			break;
		}
	}
}

static void readToken (tokenInfo *const token, void *data)
{
	int c;
	bool semi_terminator = false;

	token->type		= TOKEN_UNDEFINED;
	token->keyword	= KEYWORD_NONE;
	vStringClear (token->string);

 getNextChar:
	/* Skip whitespaces */
	do
	{
		c = getcFromInputFile ();
	}
	while (c == ' ' || c== '\t' || c == '\f' || c == '\n');

	if (c == '/')
	{
		int c0 = getcFromInputFile ();

		if (c0 == '/')
		{
			/* line comment */
			while ((c0 = getcFromInputFile ()))
			{
				if (c0 == EOF)
				{
					token->type = TOKEN_EOF;
					return;
				}
				else if (c0 == '\n')
					goto getNextChar;
			}

		}
		else if (c0 == '*')
		{
			/* block comment */
			int c1;
			while ((c1 = getcFromInputFile ()))
			{
				if (c1 == EOF)
				{
					token->type = TOKEN_EOF;
					return;
				}
				else if (c1 == '*')
				{
					int c2;
				lookingForEndOfBlockComment:
					c2 = getcFromInputFile ();
					if (c2 == EOF)
					{
						token->type = TOKEN_EOF;
						return;
					}
					else if (c2 == '/')
						goto getNextChar;
					else if (c2 == '*')
						goto lookingForEndOfBlockComment;
				}
			}
		}
		else
			ungetcToInputFile (c0);
	}

	switch (c)
	{
	case EOF:
		token->type = TOKEN_EOF;
		break;
	case '"':
		token->type = TOKEN_STRING;
		tokenPutc (token, c);
		readString (token);
		break;
	case '@':
		token->type = TOKEN_IDENTIFIER;
		tokenPutc (token, c);
		readIdentifier (token);
		break;
	case ',':
	case '}':
	case ']':
	case ')':
	case '<':
	case '>':
	case ';':
		semi_terminator = true;
	case '{':
	case '[':
	case '(':
		token->type = c;
		tokenPutc (token, c);
		break;
	default:
		if (isalpha (c) || c == '_')
		{
			tokenPutc (token, c);
			readIdentifier (token);

			token->keyword = resolveKeyword (token->string);
			if (token->keyword == KEYWORD_NONE)
				token->type = TOKEN_IDENTIFIER;
			else
				token->type = TOKEN_KEYWORD;
			break;
		}
		else
		{
			token->type = c;
			vStringPut (token->string, c);
			break;
		}
	}

	if (data)
	{
		vString *collector = data;
		if (vStringIsEmpty (collector))
			vStringCat (collector, token->string);
		else
		{
			if (!semi_terminator
				&& !strchr ("{[(", vStringLast(collector)))
				vStringPut(collector, ' ');
			vStringCat (collector, token->string);
		}
	}
}

static tokenInfo *newValaToken (void)
{
	return newToken (&valaTokenInfoClass);
}

static void parseStatement (tokenInfo *const token, int corkIndex)
{
	tokenInfo *lastToken = newValaToken ();
	bool foundSignature = false;

	do
	{
		tokenCopy (lastToken, token);
		tokenRead (token);
		if (tokenEqType (token, '('))
		{
			if (tokenIsType (lastToken, KEYWORD)) {
				tokenSkipOverPair (token);
			} else {
				vString *signature = vStringNewInit ("(");
				int corkIndex = makeSimpleTag (lastToken->string, K_METHOD);
				foundSignature = tokenSkipOverPairFull (token, signature);
				if (foundSignature)
				{
					tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
					e->extensionFields.signature = vStringDeleteUnwrap (signature);
				}
				else
					vStringDelete (signature);
			}
			break;
		}
	}
	while (!tokenIsEOF (token));

	/* Skip the body of method */
	if (foundSignature)
	{
		if (tokenSkipToType (token, '{'))
			tokenSkipOverPair (token);
	}
	tokenDelete (lastToken);
}

static void recurseValaTags (tokenInfo *token, int corkIndex)
{
	if (tokenIsKeyword(token, NAMESPACE))
		parseNamespace (token, corkIndex);
	else if (tokenIsKeyword(token, CLASS))
		parseClass (token, corkIndex);
	else if (tokenIsType (token, IDENTIFIER))
		parseStatement (token, corkIndex);
}

static void parseNamespaceBody (tokenInfo *const token, int corkIndex)
{
	do
	{
		tokenRead (token);
		if (tokenEqType (token, '}'))
			break;

		recurseValaTags (token, corkIndex);

		if (tokenEqType (token, '{'))
			tokenSkipOverPair (token);

	} while (!tokenIsEOF (token));
}

static bool readIdentifierExtended (tokenInfo *const resultToken, bool *extended)
{
	bool nextTokeIsIdentifier = false;
	tokenInfo *token = newValaToken ();
	if (extended)
		*extended = false;

	do
	{
		tokenRead (token);
		if (tokenIsTypeVal (token, '.'))
		{
			tokenPutc (resultToken, '.');
			if (extended)
				*extended = true;
		}
		else if (tokenIsType (token, IDENTIFIER))
		{
			if (tokenLast (resultToken) == '.')
				tokenCat (resultToken, token->string);
			else
			{
				tokenUnread (token);
				nextTokeIsIdentifier = true;
				break;
			}
		}
		else
		{
			if (!tokenIsEOF (token))
				tokenUnread (token);
			nextTokeIsIdentifier = false;
			break;
		}
	}
	while (1);

	tokenDelete (token);
	return nextTokeIsIdentifier;
}

static void parseClassBody (tokenInfo *const token, int classCorkIndex)
{
	bool isPublic;
	tokenInfo *typerefToken = newValaToken ();
	tokenInfo *nameToken = newValaToken ();

	do
	{
		tokenRead (token);
		if (tokenEqType (token, '}'))
			break;

		isPublic = tokenIsKeyword(token, PUBLIC);

		if (isPublic)
			tokenRead (token);

		if (tokenIsType (token, IDENTIFIER)
			|| tokenIsType (token, KEYWORD))
			tokenCopy (typerefToken, token);
		else
			break;				/* Unexpected sequence to token */

		bool typerefIsClass;
		if (!readIdentifierExtended (typerefToken, &typerefIsClass))
			goto out;

		tokenRead (token);
		if (tokenIsType (token, IDENTIFIER))
			tokenCopy (nameToken, token);

		tokenRead (token);
		int kind;
		if (tokenEqType (token, ';'))
			kind = K_FIELD;
		else if (tokenEqType (token, '{'))
			kind = K_PROP;
		else
			break;				/* Unexpected sequence of token */

		int memberCorkIndex = makeSimpleTag (nameToken->string, kind);
		tagEntryInfo *entry = getEntryInCorkQueue (memberCorkIndex);

		/* Fill access field. */
		entry->extensionFields.access = isPublic? eStrdup ("public"): NULL;
		/* Fill typeref field. */
		entry->extensionFields.typeRef [0] = eStrdup (
			typerefIsClass?
			/* '.' is included in typeref name. Can I expect it as a class?
			 */
			"class"
			:tokenIsType (typerefToken, KEYWORD)?
			/* "typename" is choice in C++ parser. However, "builtin" may be
			 * better. See #862. This should be fixed in ctags-6.0.0. */
			"typename"
			:
			/* Till we implement symbol table, we cannot resolve this.
			 * ctags-7.0.0. */
			"unknown");
		entry->extensionFields.typeRef [1] = vStringStrdup(typerefToken->string);

		/* Fill scope field. */
		entry->extensionFields.scopeIndex = classCorkIndex;

		if (kind == K_PROP)
			tokenSkipOverPair (token);
	} while (!tokenIsEOF (token));

 out:
	tokenDelete (typerefToken);
	tokenDelete (nameToken);
}

static void parseNamespace (tokenInfo *const token, int corkIndex)
{

	tokenRead (token);
	if (!tokenIsType (token, IDENTIFIER))
		return;					/* Unexpected sequence of token */

	int namespaceCorkIndex = makeSimpleTag (token->string, K_NAMESPACE);
	tagEntryInfo *entry = getEntryInCorkQueue (namespaceCorkIndex);
	entry->extensionFields.scopeIndex = corkIndex;

	tokenRead (token);
	if (!tokenSkipToType (token, '{'))
		return;					/* Unexpected sequence of token */

	parseNamespaceBody (token, namespaceCorkIndex);
}

static void parseClass (tokenInfo *const token, int corkIndex)
{
	tokenRead (token);
	if (!tokenIsType (token, IDENTIFIER))
		return;					/* Unexpected sequence of token */

	int classCorkIndex = makeSimpleTag (token->string, K_CLASS);
	tagEntryInfo *entry = getEntryInCorkQueue (classCorkIndex);
	entry->extensionFields.scopeIndex = corkIndex;

	/* Parse the class definition. */
	tokenRead (token);
	if (!tokenSkipToType (token, '{'))
		return;					/* Unexpected sequence of token */

	parseClassBody (token, classCorkIndex);
}

static void findValaTags (void)
{
	tokenInfo *const token = newValaToken ();
	do
	{
		tokenRead (token);
		recurseValaTags (token, CORK_NIL);
	}
	while (!tokenIsEOF (token));

	tokenDelete (token);
	flashTokenBacklog (&valaTokenInfoClass);
}

extern parserDefinition* ValaParser (void)
{
	static const char *const extensions [] = { "vala", NULL };

	parserDefinition* def = parserNew ("Vala");
	def->kindTable = ValaKinds;
	def->kindCount = ARRAY_SIZE (ValaKinds);
	def->extensions = extensions;
	def->keywordTable = ValaKeywordTable;
	def->keywordCount = ARRAY_SIZE (ValaKeywordTable);
	def->useCork = true;
	def->requestAutomaticFQTag = true;

	def->parser = findValaTags;
	return def;
}
