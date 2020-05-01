/*
 *   Copyright (c) 2016, Masatake YAMATO
 *   Copyright (c) 2016, Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for DTD, data type
 *   definition explained in https://www.w3.org/TR/REC-xml/#sec-physical-struct
 *
 */

#include "general.h"
#include "tokeninfo.h"

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "xtag.h"


static scopeSeparator DtdParameterEntrySeparators [] = {
	{ KIND_WILDCARD_INDEX, "/%" },
};

static scopeSeparator DtdAttSeparators [] = {
	{ KIND_WILDCARD_INDEX, "/@" },
};

typedef enum {
	DTD_PARAMETER_ENTITY_ELEMENT_NAME,
	DTD_PARAMETER_ENTITY_CONDITION,
	DTD_PARAMETER_ENTITY_PART_OF_ATT_DEF,
} dtdEntityRole;

static roleDefinition DtdEntityRoles [] = {
	{ true, "elementName", "element names" },
	{ true, "condition",    "conditions" },
	{ true, "partOfAttDef", "part of attribute definition" },
};

typedef enum {
	DTD_ELEMENT_ATT_OWNER,
} dtdElementRole;

static roleDefinition DtdElementRoles [] = {
	{ true, "attOwner", "attributes owner" },
};

typedef enum {
	K_ENTITY,
	K_PARAMETER_ENTITY,
	// K_EXTERNAL_ENTITY,
	// K_UNPARSED_ENTITY,
	K_ELEMENT,
	K_ATTRIBUTE,
	K_NOTATION,
} dtdKind;

static kindDefinition DtdKinds [] = {
	{ true, 'E', "entity",    "entities" },
	{ true, 'p', "parameterEntity", "parameter entities",
	  .referenceOnly = false, ATTACH_ROLES(DtdEntityRoles),
	  ATTACH_SEPARATORS(DtdParameterEntrySeparators),
	},
	// { true, 'X', "externalEntity", "external entities" },
	// { true, 'U', "unparsedEntity", "unparsed entities" },
	{ true, 'e', "element",   "elements",
	  .referenceOnly = false, ATTACH_ROLES(DtdElementRoles) },
	{ true, 'a', "attribute", "attributes",
	  ATTACH_SEPARATORS(DtdAttSeparators), },
	{ true, 'n', "notation", "notations" },

};

enum {
	KEYWORD_ENTITY,
	KEYWORD_ELEMENT,
	KEYWORD_ATTLIST,
	KEYWORD_INCLUDE,
	KEYWORD_IGNORE,
	// KEYWORD_PUBLIC,
	// KEYWORD_SYSTEM,
	KEYWORD_NOTATION,
	KEYWORD_FIXED,
	KEYWORD_ATTR_TYPES,
	KEYWORD_ATTR_DEFAULT_DECLS,
};

typedef int keywordId;

static const keywordTable DtdKeywordTable[] = {
	{ "ENTITY",    KEYWORD_ENTITY   },
	{ "ELEMENT",   KEYWORD_ELEMENT  },
	{ "ATTLIST",   KEYWORD_ATTLIST },
	{ "INCLUDE",   KEYWORD_INCLUDE  },
	{ "IGNORE",    KEYWORD_IGNORE   },
	// { "PUBLIC",    KEYWORD_PUBLIC   },
	// { "SYSTEM",    KEYWORD_SYSTEM   },
	{ "NOTATION",  KEYWORD_NOTATION },
	{ "FIXED",     KEYWORD_FIXED    },
	{ "CDATA",     KEYWORD_ATTR_TYPES },
	{ "ID",        KEYWORD_ATTR_TYPES },
	{ "IDREF",     KEYWORD_ATTR_TYPES },
	{ "IDREFS",    KEYWORD_ATTR_TYPES },
	{ "ENTITIES",  KEYWORD_ATTR_TYPES },
	{ "NMTOKEN",   KEYWORD_ATTR_TYPES },
	{ "NMTOKENS",  KEYWORD_ATTR_TYPES },
	{ "REQUIRED",  KEYWORD_ATTR_DEFAULT_DECLS },
	{ "IMPLIED",   KEYWORD_ATTR_DEFAULT_DECLS },
};

enum eTokenType {
	/* 0..255 are the byte's value */
	TOKEN_CLOSE = '>',
	TOKEN_EOF = 256,
	TOKEN_UNDEFINED,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
	TOKEN_OPEN,					/* <! */
	TOKEN_STRING,
};

static void readToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED);
static void clearToken (tokenInfo *token);
static void copyToken (tokenInfo *dest, tokenInfo *src, void *data CTAGS_ATTR_UNUSED);

typedef struct sDtdToken {
	tokenInfo base;
	int scopeIndex;
} dtdToken;

#define DTD(TOKEN) ((dtdToken *)TOKEN)

static struct tokenInfoClass dtdTokenInfoClass = {
	.nPreAlloc = 16,
	.typeForUndefined = TOKEN_UNDEFINED,
	.keywordNone      = KEYWORD_NONE,
	.typeForKeyword   = TOKEN_KEYWORD,
	.typeForEOF       = TOKEN_EOF,
	.extraSpace       = sizeof (dtdToken) - sizeof (tokenInfo),
	.read             = readToken,
	.clear            = clearToken,
	.copy             = copyToken,
};

static langType Lang_dtd;

#define isIdentifierChar(c) (isalnum (c) || c == '-' || c == '_' || c == '.' \
							 || c == ':')

static tokenInfo *newDtdToken (void)
{
	return newToken (&dtdTokenInfoClass);
}

static void clearToken (tokenInfo *token)
{
	DTD (token)->scopeIndex = CORK_NIL;
}

static void copyToken (tokenInfo *dest, tokenInfo *src, void *data CTAGS_ATTR_UNUSED)
{
	DTD (dest)->scopeIndex = DTD (src)->scopeIndex;
}

static void readToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED)
{
	int c, c0;

	token->type		= TOKEN_UNDEFINED;
	token->keyword	= KEYWORD_NONE;
	vStringClear (token->string);

 retry:
	do {
		c = getcFromInputFile ();
	} while (c == ' ' || c == '\t' || c == '\f' || c == '\n');

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	switch (c)
	{
	case EOF:
		token->type = TOKEN_EOF;
		break;
	case ';':
	case '&':
	case '%':
	case '>':
	case '#':
	case '?':
	case '[':
	case ']':
	case '|':
	case ',':
	case '(':
	case ')':
	case '+':
		token->type = c;
		break;
	case '<':
		c0 = getcFromInputFile();
		if (c0 == '!')
		{
			token->type = TOKEN_OPEN;
			break;
		}
		else
		{
			ungetcToInputFile (c0);
			token->type = c;
			break;
		}
	case '-':
		c0 = getcFromInputFile();
		if (c0 == '-')
		{
			int c1, c2;

			while ( (c1 = getcFromInputFile()) != EOF )
			{
				if (c1 == '-')
				{
					c2 = getcFromInputFile();
					if (c2 == '-' || c2 == EOF)
						goto retry;
				}
			}
		}
		else
		{
			ungetcToInputFile (c0);
			token->type = c;
		}
		break;
	case '"':
	case '\'':
		token->type = TOKEN_STRING;
		while ((c0 = getcFromInputFile ()))
		{
			if (c0 == EOF || c0 == c)
				break;
			else
				tokenPutc(token, c0);
		}
		break;
	default:
		if (isIdentifierChar(c))
		{
			tokenPutc(token, c);
			while ((c = getcFromInputFile ()))
			{
				if (isIdentifierChar(c))
					tokenPutc(token, c);
				else
				{
					ungetcToInputFile (c);
					break;
				}
			}
			token->keyword = lookupKeyword (vStringValue (token->string),
											Lang_dtd);
			if (token->keyword == KEYWORD_NONE)
				token->type = TOKEN_IDENTIFIER;
			else
				token->type = TOKEN_KEYWORD;

		}
		else
			token->type = c;
		break;
	}
}

static int makeDtdTagMaybe (tagEntryInfo *const e, tokenInfo *const token,
							int kind, int role)
{
	if (role == ROLE_DEFINITION_INDEX)
	{
		if (! DtdKinds[kind].enabled)
			return CORK_NIL;
	}
	else if (! (isXtagEnabled (XTAG_REFERENCE_TAGS)
				&& DtdKinds[kind].roles[role].enabled))
		return CORK_NIL;

	initRefTagEntry (e, tokenString (token),
					 kind,
					 role);
	e->lineNumber = token->lineNumber;
	e->filePosition = token->filePosition;
	e->extensionFields.scopeIndex = DTD (token)->scopeIndex;

	return makeTagEntry (e);
}

static void backpatchEndField (int index, unsigned long lineNumber)
{
	tagEntryInfo *ep = getEntryInCorkQueue (index);

	if (ep)
		ep->extensionFields.endLine = lineNumber;
}

static void parseEntity (tokenInfo *const token)
{
	tagEntryInfo e;
	int index = CORK_NIL;

	tokenRead (token);
	if (token->type == '%')
	{
		tokenRead (token);
		if (tokenIsType(token, IDENTIFIER))
			index = makeDtdTagMaybe (&e, token,
									 K_PARAMETER_ENTITY, ROLE_DEFINITION_INDEX);
	}
	else if (tokenIsType(token, IDENTIFIER))
		index = makeDtdTagMaybe (&e, token,
								 K_ENTITY, ROLE_DEFINITION_INDEX);

	if (tokenSkipToType (token, TOKEN_CLOSE) && (index != CORK_NIL))
		backpatchEndField (index, token->lineNumber);
}

static tokenInfo *parserParameterEntityRef (tokenInfo *const token)
{
	tokenRead (token);
	if (tokenIsType(token, IDENTIFIER))
	{
		tokenInfo * identifier = newTokenByCopying (token);

		tokenRead (token);

		if (token->type == ';')
			return identifier;
		else
		{
			tokenDelete (identifier);
			return NULL;
		}
	}
	return NULL;
}

static void parseElement (tokenInfo *const token, bool skipToClose)
{
	tagEntryInfo e;
	int original_index;

	if (skipToClose)
		original_index = (int)countEntryInCorkQueue ();

	tokenRead (token);
	if (token->type == '%')
	{
		tokenInfo * identifier = parserParameterEntityRef (token);
		if (identifier)
		{
			makeDtdTagMaybe (&e, identifier,
							 K_PARAMETER_ENTITY,
							 DTD_PARAMETER_ENTITY_ELEMENT_NAME);
			tokenDelete (identifier);
		}
	}
	else if (tokenIsType(token, IDENTIFIER))
		makeDtdTagMaybe (&e, token, K_ELEMENT, ROLE_DEFINITION_INDEX);
	else if (token->type == '(')
	{
		do {
			parseElement (token, false);
		} while ((!tokenIsEOF (token))
				 && (token->type != ')'));
	}

	if (skipToClose)
	{
		int current_index = (int)countEntryInCorkQueue ();
		if (tokenSkipToType (token, TOKEN_CLOSE)
			&& (current_index > original_index))
		{
			for (int index = original_index; index < current_index; index++)
				backpatchEndField (index, token->lineNumber);
		}
	}
}

static void parseAttDefs (tokenInfo *const token)
{
	/*  [53]   	AttDef	   ::=   	S Name S AttType S DefaultDecl */

	do {
		tokenRead (token);

		/* Name */
		if (tokenIsType(token, IDENTIFIER))
		{
			tagEntryInfo e;
			makeDtdTagMaybe (&e, token,
							 K_ATTRIBUTE, ROLE_DEFINITION_INDEX);
		}
		else if (tokenIsKeyword(token, ATTR_TYPES)
				 || tokenIsKeyword(token, ENTITY))
			/* AttType -> just consuming */
			;
		else if (tokenIsKeyword(token, NOTATION))
		{
			/* AttType -> just consuming */
			tokenRead (token);
			if (token->type == '(')
				tokenSkipToType (token, ')');
		}
		else if (token->type == '(')
		{
			/* AttType, TODO: Enumerated members can be tagged. */
			tokenSkipToType (token, ')');
		}
		else if (token->type == '#')
		{
			/* DefaultDecl */
			tokenRead (token);
			if (tokenIsKeyword(token, FIXED))
				tokenRead (token);
			else if (tokenIsKeyword(token, ATTR_DEFAULT_DECLS))
			{
				/* Just consuming */
			}
		}
		else if (tokenIsType (token, STRING))
			;					/* DefaultDecl -> Just consuming */
		else if (token->type == '%')
		{
			tokenInfo * identifier = parserParameterEntityRef (token);
			if (identifier)
			{
				tagEntryInfo e;
				makeDtdTagMaybe (&e, identifier,
								 K_PARAMETER_ENTITY,
								 DTD_PARAMETER_ENTITY_PART_OF_ATT_DEF);
				tokenDelete (identifier);
			}
		}
		else if (tokenIsType(token, CLOSE))
		{
			DTD (token)->scopeIndex = CORK_NIL;
			tokenUnread (token);
			break;
		}
	} while (!tokenIsEOF (token));
}

static void parseAttlist (tokenInfo *const token)
{
	tagEntryInfo e;
	int index = CORK_NIL;

	tokenRead (token);
	if (token->type == '%')
	{
		tokenRead (token);
		if (tokenIsType(token, IDENTIFIER))
		{
			tokenInfo * identifier = parserParameterEntityRef (token);
			if (identifier)
			{
				index = makeDtdTagMaybe (&e, identifier,
										 K_ENTITY,
										 DTD_PARAMETER_ENTITY_ELEMENT_NAME);
				tokenDelete (identifier);

				DTD (token)->scopeIndex = index;
				parseAttDefs (token);
				DTD (token)->scopeIndex = CORK_NIL;
			}
		}
	}
	else if (tokenIsType(token, IDENTIFIER))
	{
		tokenInfo * element = newTokenByCopying (token);

		index = makeDtdTagMaybe (&e, element,
								 K_ELEMENT, DTD_ELEMENT_ATT_OWNER);
		tokenDelete (element);

		DTD (token)->scopeIndex = index;
		parseAttDefs (token);
		DTD (token)->scopeIndex = CORK_NIL;
	}

	tokenSkipToType (token, TOKEN_CLOSE);
	backpatchEndField (index, token->lineNumber);
}

static void parseNotation (tokenInfo *const token)
{
	int index = CORK_NIL;
	tagEntryInfo e;

	tokenRead (token);
	if (tokenIsType(token, IDENTIFIER))
		index = makeDtdTagMaybe (&e, token,
								 K_NOTATION, ROLE_DEFINITION_INDEX);

	tokenSkipToType (token, TOKEN_CLOSE);
	backpatchEndField (index, token->lineNumber);
}


static void parseSection (tokenInfo *const token);

static void parseDtdTag1 (tokenInfo *const token)
{
	if (tokenIsType(token, OPEN))
	{
		tokenRead (token);
		if (tokenIsKeyword (token, ELEMENT))
			parseElement(token, true);
		else if (tokenIsKeyword (token, ATTLIST))
			parseAttlist(token);
		else if (tokenIsKeyword (token, ENTITY))
			parseEntity(token);
		else if (tokenIsKeyword (token, NOTATION))
			parseNotation(token);
		else if (token->type == '[')
		{
			tokenRead (token);
			parseSection (token);
			tokenSkipToType (token, ']');
		}
		else if (!tokenIsType(token, CLOSE))
			tokenSkipToType (token, TOKEN_CLOSE);
	}
}

static void parseSection (tokenInfo *const token)
{
	if (tokenIsKeyword(token, IGNORE))
		tokenSkipToType (token, ']');
	else
	{
		if (tokenIsKeyword (token, INCLUDE))
		{
			tokenRead (token);
			if (token->type == '[')
			{
				do {
					tokenRead (token);
				} while ((!tokenIsEOF (token))
						 && (token->type != ']'));
			}
		}
		else if (token->type == '%')
		{
			tokenInfo *const condition = parserParameterEntityRef (token);
			if (condition)
			{
				tagEntryInfo e;
				int index = makeDtdTagMaybe (&e, condition,
											 K_PARAMETER_ENTITY,
											 DTD_PARAMETER_ENTITY_CONDITION);
				tokenDelete (condition);
				tokenRead (token);
				if (token->type == '[')
				{
					do {
						tokenRead (token);
						parseDtdTag1 (token);
					} while ((!tokenIsEOF (token))
							 && (token->type != ']'));
					if (token->type== ']')
						backpatchEndField (index, token->lineNumber);
				}
			}
		}
	}
}

static void findDtdTags (void)
{
	tokenInfo *const token = newDtdToken ();

	do {
		tokenRead (token);
		parseDtdTag1 (token);
	} while (!tokenIsEOF (token));

	tokenDelete (token);

	flashTokenBacklog (&dtdTokenInfoClass);
}

static void initialize (const langType language)
{
	Lang_dtd = language;
}

extern parserDefinition* DtdParser (void)
{
	parserDefinition* def = parserNew ("DTD");

	/* File name patters are picked from Linux kernel. */
	static const char *const extensions [] = {
		"dtd",
		"mod",
		NULL
	};

	def->initialize = initialize;
	def->parser     = findDtdTags;

	def->kindTable      = DtdKinds;
	def->kindCount  = ARRAY_SIZE (DtdKinds);
	def->extensions = extensions;

	def->keywordTable = DtdKeywordTable;
	def->keywordCount = ARRAY_SIZE (DtdKeywordTable);

	def->useCork    = CORK_QUEUE;
	def->requestAutomaticFQTag = true;

	return def;
}
