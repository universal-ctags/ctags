/*
*   Copyright (c) 2019, Alberto Fanjul <albertofanjul@gmail.com>
*   Copyright (c) 2020, Masatake Yamato <yamato@redhat.com>
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning Vala source files.
*   https://www.vala-project.org/doc/vala/
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
	KEYWORD_BUILTIN_TYPE,
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

static struct keywordGroup valaBuiltInKeywordGroup = {
	.value = KEYWORD_BUILTIN_TYPE,
	.addingUnlessExisting = false,
	.keywords = {
		/* type:
		 *   value-type:
		 *     fundamental-struct-type:
		 *       integral-type: */
		"char",
		"uchar",
		"short",
		"ushort",
		"int",
		"uint",
		"long",
		"ulong",
		"size_t",
		"ssize_t",
		"int8",
		"uint8",
		"int16",
		"uint16",
		"int32",
		"uint32",
		"int64",
		"uint64",
		"unichar",
		/* type:
		 *   value-type:
		 *     fundamental-struct-type:
		 *       floating-point-type: */
		"float",
		"double",
		/* type:
		 *   value-type:
		 *     fundamental-struct-type: */
		"bool",
		/* type:
		 *   reference-type: */
		"string",
		NULL
	},
};

static const keywordTable ValaKeywordTable [] = {
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

typedef enum {
	F_PROPERTIES,
} valaField;

static fieldDefinition ValaFields[] = {
	{ .name = "properties",
	  .description = "properties (static)",
	  .enabled = true },
};

/*
*   FUNCTION PROTOTYPES
*/

static void readToken (tokenInfo *const token, void *data);
static void parseNamespace (tokenInfo *const token, int corkIndex);
static void parseInterface (tokenInfo *const token, int corkIndex);
static void parseClass (tokenInfo *const token, int kindIndex, int corkIndex);
static int  parseStatement (tokenInfo *const token, int corkIndex);
static void parseEnum (tokenInfo *const token, int kindIndex, int elementKindIndex, int corkIndex);
static void recurseValaTags (tokenInfo *token, int parentIndex);


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
					token->lineNumber = getInputLineNumber ();
					token->filePosition = getInputFilePosition ();
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
					token->lineNumber = getInputLineNumber ();
					token->filePosition = getInputFilePosition ();
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
						token->lineNumber = getInputLineNumber ();
						token->filePosition = getInputFilePosition ();
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

	token->lineNumber = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();
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

static int parseStatement (tokenInfo *const token, int parentIndex)
{
	tokenInfo *lastToken = newValaToken ();
	bool foundSignature = false;
	tagEntryInfo *e = NULL;
	int corkIndex = CORK_NIL;

	do
	{
		tokenCopy (lastToken, token);
		tokenRead (token);
		if (tokenIsTypeVal (token, '('))
		{
			if (tokenIsType (lastToken, KEYWORD))
				tokenSkipOverPair (token);
			else
			{
				corkIndex = makeSimpleTagFromToken (lastToken, K_METHOD, parentIndex);
				e = getEntryInCorkQueue (corkIndex);
				if (e)
				{
					vString *signature = vStringNewInit ("(");
					foundSignature = tokenSkipOverPairFull (token, signature);
					if (foundSignature)
						e->extensionFields.signature = vStringDeleteUnwrap (signature);
					else
						vStringDelete (signature);
				}
			}
			break;
		}
	}
	while (!tokenIsTypeVal (token, ';') && !tokenIsEOF (token));

	/* Skip the body of method */
	if (foundSignature)
	{
		if (tokenSkipToType (token, '{'))
			tokenSkipOverPair (token);
	}

	if (e)
		e->extensionFields.endLine = token->lineNumber;

	tokenDelete (lastToken);

	return corkIndex;
}

static void parseEnumBody (tokenInfo *const token, int kindIndex, int corkIndex)
{
	bool s = trianglePairState;
	trianglePairState = false;
	while (!tokenIsEOF (token))
	{
		tokenRead (token);
		if (tokenIsType (token, IDENTIFIER))
		{
			makeSimpleTagFromToken (token, kindIndex, corkIndex);
			tokenType endMakers [] = {',', ';', '}'};
			if (tokenSkipToTypesOverPairs (token, endMakers, ARRAY_SIZE(endMakers)))
			{
				if (tokenIsTypeVal (token, ','))
				{
					tokenRead (token);
					if (!tokenIsTypeVal (token, '}'))
					{
						tokenUnread (token);
						continue;
					}
				}

				if (tokenIsTypeVal (token, '}'))
					break;
				else if (tokenIsTypeVal (token, ';'))
				{
					bool t = trianglePairState;
					do
					{
						tokenRead (token);
						if (tokenIsTypeVal (token, '}'))
							break;
						recurseValaTags (token, corkIndex);
					} while (!tokenIsEOF (token));
					trianglePairState = t;
					break;
				}
			}
			else
				break;
		}
	}
	trianglePairState = s;
}

static void parseEnum (tokenInfo *const token, int kindIndex, int elementKindIndex, int corkIndex)
{
	tokenRead (token);
	if (!tokenIsType (token, IDENTIFIER))
		return;					/* Unexpected sequence of token */

	int enumCorkIndex = makeSimpleTagFromToken (token, kindIndex, corkIndex);

	tokenRead (token);
	if (!tokenSkipToType (token, '{'))
		return;					/* Unexpected sequence of token */
	parseEnumBody (token, elementKindIndex, enumCorkIndex);

	tagEntryInfo *e = getEntryInCorkQueue (enumCorkIndex);
	if (e)
		e->extensionFields.endLine = token->lineNumber;
}

static void recurseValaTags (tokenInfo *token, int parentIndex)
{
	/* Skip attributes */
	if (tokenIsTypeVal (token, '['))
		tokenSkipOverPair (token);
	else if (tokenIsKeyword(token, NAMESPACE))
		parseNamespace (token, parentIndex);
	else if (tokenIsKeyword(token, INTERFACE))
		parseInterface (token, parentIndex);
	else if (tokenIsKeyword(token, CLASS))
		parseClass (token, K_CLASS, parentIndex);
	else if (tokenIsKeyword(token, ENUM))
		parseEnum (token, K_ENUM, K_ENUMVALUE, parentIndex);
	else if (tokenIsKeyword(token, ERRORDOMAIN))
		parseEnum (token, K_ERRORDOMAIN, K_ERRORCODE, parentIndex);
	else if (tokenIsKeyword (token, STRUCT))
		parseClass (token, K_STRUCT, parentIndex);
	else if (tokenIsType (token, IDENTIFIER))
		parseStatement (token, parentIndex);
}

static void parseNamespaceBody (tokenInfo *const token, int parentIndex)
{
	do
	{
		tokenRead (token);
		if (tokenIsTypeVal (token, '}'))
			break;

		recurseValaTags (token, parentIndex);

		if (tokenIsTypeVal (token, '{'))
			tokenSkipOverPair (token);

	} while (!tokenIsEOF (token));
}

static bool readIdentifierExtended (tokenInfo *const resultToken, bool *extended)
{
	bool nextTokenIsIdentifier = false;
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
		else if (tokenIsType (token, KEYWORD))
			; /* Skip keywords */
		else if (tokenIsTypeVal (token, '?'))
			; /* Skip nullable */
		else if (tokenIsTypeVal (token, '*'))
			; /* Skip pointer or indirection */
		else if (tokenIsTypeVal (token, '<'))
			/* Skip over generic type parameter. */
			tokenSkipOverPair (token);
		else if (tokenIsTypeVal (token, '['))
			/* Skip over array. */
			tokenSkipOverPair (token);
		else if (tokenIsType (token, IDENTIFIER))
		{
			if (tokenLast (resultToken) == '.')
				tokenCat (resultToken, token->string);
			else
			{
				tokenUnread (token);
				nextTokenIsIdentifier = true;
				break;
			}
		}
		else
		{
			if (!tokenIsEOF (token))
				tokenUnread (token);
			nextTokenIsIdentifier = false;
			break;
		}
	}
	while (1);

	tokenDelete (token);
	return nextTokenIsIdentifier;
}

static void parseClassBody (tokenInfo *const token, int classCorkIndex)
{
	char *visiblity = NULL;
	tokenInfo *typerefToken = newValaToken ();
	tokenInfo *nameToken = newValaToken ();
	const char *className = NULL;

	{
		tagEntryInfo *e = getEntryInCorkQueue (classCorkIndex);
		if (e)
			className = e->name;
	}

	do
	{
		bool seen_static = false;
		tokenRead (token);
		if (tokenIsTypeVal (token, '}'))
			break;

		if (tokenIsKeyword(token, PUBLIC) || tokenIsKeyword (token, PROTECTED) ||
			tokenIsKeyword (token, PRIVATE) || tokenIsKeyword (token, INTERNAL))
		{
			visiblity = eStrdup (tokenString (token));
			tokenRead (token);
		}

		if (tokenIsKeyword(token, STATIC))
		{
			seen_static = true;
			tokenRead (token);
		}
		else if (tokenIsKeyword(token, CONSTRUCT))
		{
			tokenRead (token);
			if (tokenIsTypeVal (token, '{'))
			{
				/* TODO: we can make an anonymous tag for the constructor.  */
				tokenSkipOverPair (token);
				continue;
			}
		}
		else if (tokenIsKeyword(token, CONST))
		{
			/* TODO: we can record "const" to "properties:" field. */
			tokenRead (token);
		}

		if (tokenIsType (token, IDENTIFIER)
			|| tokenIsType (token, KEYWORD))
			tokenCopy (typerefToken, token);
		else
		{
			if (visiblity)
				eFree (visiblity);
			break;				/* Unexpected sequence to token */
		}

		bool typerefIsClass;
		readIdentifierExtended (typerefToken, &typerefIsClass);

		bool nameFound = false;
		tokenRead (token);
		if (tokenIsType (token, IDENTIFIER))
		{
			tokenCopy (nameToken, token);
			nameFound = true;
			tokenRead (token);
		}

		int kind = KIND_GHOST_INDEX;
		int methodIndex = CORK_NIL;
		bool is_name_constructor = false;
		if (tokenIsTypeVal (token, '('))
		{
			if (nameFound)
			{
				/* Method */
				tokenUnread(token);
				tokenCopy (token, nameToken);
				methodIndex = parseStatement (token, classCorkIndex);
				tagEntryInfo *e = getEntryInCorkQueue (methodIndex);
				if (e && e->kindIndex == K_METHOD)
					kind = e->kindIndex;
			}
			else if (strcmp (vStringValue (typerefToken->string), className) == 0)
			{
				/* Constructor */
				tokenUnread(token);
				tokenCopy (token, typerefToken);
				is_name_constructor = true;
				methodIndex = parseStatement (token, classCorkIndex);
				tagEntryInfo *e = getEntryInCorkQueue (methodIndex);
				if (e && e->kindIndex == K_METHOD)
					kind = e->kindIndex;
			}
			else
			{
				tokenSkipOverPair (token);
				tokenRead (token);
			}
		}
		else if (tokenIsTypeVal (token, ';')
			|| tokenIsTypeVal (token, '='))
		{
			kind = K_FIELD;
			if (tokenIsTypeVal (token, '='))
			{
				bool s = trianglePairState;
				tokenSkipToTypeOverPairs (token, ';');
				trianglePairState = s;
			}
		}
		else if (tokenIsTypeVal (token, '{'))
			kind = K_PROP;
		else
			break;				/* Unexpected sequence of token */

		int memberCorkIndex = methodIndex == CORK_NIL
			? makeSimpleTagFromToken (nameToken, kind, classCorkIndex)
			: methodIndex;
		tagEntryInfo *entry = getEntryInCorkQueue (memberCorkIndex);

		/* Fill access field. */
		entry->extensionFields.access = visiblity;
		visiblity = NULL;
		if (!is_name_constructor)
		{
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
		}
		/* Fill prototypes field. */
		if (seen_static)
			attachParserField(entry, ValaFields[F_PROPERTIES].ftype, "static");

		if (kind == K_PROP)
			tokenSkipOverPair (token);
		if (kind != K_METHOD)
			entry->extensionFields.endLine = token->lineNumber;
	} while (!tokenIsEOF (token));

	if (visiblity)
		eFree (visiblity);
	tokenDelete (typerefToken);
	tokenDelete (nameToken);
}

static void parseNamespace (tokenInfo *const token, int parentIndex)
{

	tokenRead (token);
	if (!tokenIsType (token, IDENTIFIER))
		return;					/* Unexpected sequence of token */

	int namespaceCorkIndex = makeSimpleTagFromToken (token, K_NAMESPACE, parentIndex);

	tokenRead (token);
	if (!tokenSkipToType (token, '{'))
		return;					/* Unexpected sequence of token */

	parseNamespaceBody (token, namespaceCorkIndex);
	tagEntryInfo *e = getEntryInCorkQueue (namespaceCorkIndex);
	if (e)
		e->extensionFields.endLine = token->lineNumber;
}

static void parseInterface (tokenInfo *const token, int parentIndex)
{

	tokenRead (token);
	if (!tokenIsType (token, IDENTIFIER))
		return;					/* Unexpected sequence of token */

	int interfaceCorkIndex = makeSimpleTagFromToken (token, K_INTERFACE, parentIndex);

	tokenRead (token);
	if (!tokenSkipToType (token, '{'))
		return;					/* Unexpected sequence of token */

	parseClassBody (token, interfaceCorkIndex);	/* Should we have a custom parser? */
	tagEntryInfo *e = getEntryInCorkQueue (interfaceCorkIndex);
	if (e)
		e->extensionFields.endLine = token->lineNumber;
}

static void parseInheritanceList (tokenInfo *const token, int classIndex)
{
	vString *list = vStringNew ();

	do
	{
		tokenRead (token);
		if (!tokenIsType (token, IDENTIFIER))
			break;				/* Unexpected sequence of token */
		readIdentifierExtended (token, NULL);
		vStringCat (list, token->string);

		tokenRead (token);
		if (tokenIsTypeVal (token, ','))
			vStringPut (list, ',');
		else if (tokenIsTypeVal (token, '{'))
		{
			tagEntryInfo *e = getEntryInCorkQueue (classIndex);
			if (e)
			{
				e->extensionFields.inheritance = vStringDeleteUnwrap (list);
				list = NULL;
			}
			break;
		}
		else
			break;			 /* Unexpected sequence of token or EOF */
	} while (1);

	vStringDelete (list);		/* NULL is acceptable */
}

static void parseClass (tokenInfo *const token, int kindIndex, int parentIndex)
{
	tokenRead (token);
	if (!tokenIsType (token, IDENTIFIER))
		return;					/* Unexpected sequence of token */
	readIdentifierExtended (token, NULL);

	int classCorkIndex = makeSimpleTagFromToken (token, kindIndex, parentIndex);

	/* Parse the class definition. */
	tokenRead (token);
	if (tokenIsTypeVal (token, ':'))
		parseInheritanceList (token, classCorkIndex);

	if (!tokenSkipToType (token, '{'))
		return;					/* Unexpected sequence of token */

	parseClassBody (token, classCorkIndex);
	tagEntryInfo *e = getEntryInCorkQueue (classCorkIndex);
	if (e)
		e->extensionFields.endLine = token->lineNumber;
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

static void initialize (const langType language)
{
	addKeywordGroup (&valaBuiltInKeywordGroup, language);
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
	def->fieldTable = ValaFields;
	def->fieldCount = ARRAY_SIZE (ValaFields);
	def->useCork = true;
	def->requestAutomaticFQTag = true;

	def->initialize = initialize,
	def->parser = findValaTags;
	return def;
}
