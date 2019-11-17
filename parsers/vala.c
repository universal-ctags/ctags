/*
*   Copyright (c) 2019, <YOURNAME HERE>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning Vaka source files.
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
	K_FIELD,
	K_METHOD,
	K_PROP,
} valaKind;

static kindDefinition ValaKinds [] = {
	{ true,  'c', "class",      "classes"},
	{ true,  'f', "field",      "fields"},
	{ true,  'm', "method",     "methods"},
	{ true,  'p', "property",   "properties"},
};

enum eKeywordId
{
	KEYWORD_CLASS,
	KEYWORD_PUBLIC,
	KEYWORD_STRING,
	KEYWORD_VOID,
};

typedef int keywordId; /* to allow KEYWORD_NONE */

static const keywordTable ValaKeywordTable [] = {
	{ "class",  KEYWORD_CLASS  },
	{ "public", KEYWORD_PUBLIC },
	{ "string", KEYWORD_STRING },
	{ "void",   KEYWORD_VOID  },
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


/*
*   DATA DEFINITIONS
*/

static struct tokenTypePair typePairs [] = {
	{ '{', '}', NULL },
	{ '[', ']', NULL },
	{ '(', ')', NULL },
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
	bool semi_terminaotr = false;

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
	case ';':
		semi_terminaotr = true;
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
			if (!semi_terminaotr
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

static void parseStatement (tokenInfo *const token)
{
	tokenInfo *lastToken = newValaToken ();
	bool foundSignature = false;

	do
	{
		tokenCopy (lastToken, token);
		tokenRead (token);
		if (tokenEqType (token, '('))
		{
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

		isPublic = tokenIsKeyword(token, PUBLIC)? true: false;

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

static void parseClass (tokenInfo *const token)
{
	tokenRead (token);
	if (!tokenIsType (token, IDENTIFIER))
		return;					/* Unexpected sequence of token */

	int classCorkIndex = makeSimpleTag (token->string, K_CLASS);

	/* Skip the class definition. */
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
		if (tokenIsKeyword(token, CLASS))
			parseClass (token);
		else if (tokenIsType (token, IDENTIFIER)
			|| tokenIsType (token, KEYWORD))
			parseStatement (token);
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
