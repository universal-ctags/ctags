/*
 * containerfile.ctags --- regex parser for Containerfile and Dockerfile
 *
 * Copyright (c) 2023, 2024, Red Hat, Inc.
 * Copyright (c) 2023, 2024, Masatake YAMATO
 *
 * Author: Masatake YAMATO <yamato@redhat.com>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
 * USA.
 *
 * Reference:
 * - https://docs.docker.com/engine/reference/builder/
 * - https://github.com/containers/common/blob/main/docs/Containerfile.5.md
 * - https://github.com/containers/podman/blob/main/docs/source/markdown/podman-build.1.md.in
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include "kind.h"
#include "parse.h"
#include "keyword.h"
#include "tokeninfo.h"
#include "field.h"
#include "read.h"
#include "entry.h"

#include "cpreprocessor.h"

/*
 *   DATA DEFINITIONS
 */
typedef enum {
	K_IMAEG,
	K_ARG,
	K_ENV,
	K_LABEL,
} containerfileKind;

typedef enum {
	R_IMAGE_BASE,
} containerfileImageRole;

static roleDefinition ContainerfileImageRoles [] = {
	{ true, "base", "referenced as a base image in FROM directive" },
};

static kindDefinition ContainerfileKinds [] = {
	{ true, 'i', "image", "images referred or defined with FROM directive",
	  .referenceOnly = false, ATTACH_ROLES (ContainerfileImageRoles) },
	{ true, 'a', "arg",   "objects defined with ARG directive", },
	{ true, 'e', "env",   "objects defined with ENV directive", },
	{ true, 'l', "label", "objects defined with LABEL directive", },
};

typedef enum eKeywordId {
	KEYWORD_FROM,
	KEYWORD_AS,
	KEYWORD_ARG,
	KEYWORD_ENV,
	KEYWORD_LABEL,
} keywordId;

static const keywordTable ContainerfileKeywordTable [] = {
	{ "FROM",  KEYWORD_FROM  },
	{ "AS",    KEYWORD_AS    },
	{ "ARG",   KEYWORD_ARG   },
	{ "ENV",   KEYWORD_ENV   },
	{ "LABEL", KEYWORD_LABEL },
};

enum eTokenType {
	TOKEN_EOF,
	TOKEN_NEWLINE,
	TOKEN_KEYWORD,
	TOKEN_ID,
	TOKEN_EQ,
	TOKEN_UNDEFINED,
};

static langType Lang_containerfile;

/*
 * Function declarations
 */

static void readToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED);

/*
 * Function definitions
 */

static struct tokenInfoClass containerfileTokenInfoClass = {
	.nPreAlloc = 1,
	.typeForUndefined = TOKEN_UNDEFINED,
	.keywordNone      = KEYWORD_NONE,
	.typeForKeyword   = TOKEN_KEYWORD,
	.typeForEOF       = TOKEN_EOF,
	.read             = readToken,
};

static void readId(tokenInfo *const token)
{
	while (true)
	{
		int c = cppGetc ();
		switch (c)
		{
		case EOF:
			return;

		case ' ':
		case '\t':
		case '\r':
		case '\f':
			return;

		case '\n':
			cppUngetc (c);
			return;

		case CPP_STRING_SYMBOL:
		case CPP_CHAR_SYMBOL:
			tokenCat (token, cppGetLastCharOrStringContents());
			break;

		case '\\':
			c = cppGetc ();
			if (c == EOF)
			{
				/* broken input */
				return;
			}
			if (c == CPP_STRING_SYMBOL || c == CPP_CHAR_SYMBOL)
			{
				/* Broken input */
				tokenCat (token, cppGetLastCharOrStringContents());
			}
			else
				tokenPutc (token, c);
			break;
		case '=':
			cppUngetc (c);
			return;
		default:
			tokenPutc (token, c);
			break;
		}
	}
}

static void readToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED)
{
	token->type		= TOKEN_UNDEFINED;
	token->keyword	= KEYWORD_NONE;
	vStringClear (token->string);

	int c;

	do
		c = cppGetc ();
	while (c == ' ' || c == '\t' || c == '\f' || c == '\r');

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

 	switch (c)
	{
	case EOF:
		token->type = TOKEN_EOF;
		break;
	case '\n':
		token->type = TOKEN_NEWLINE;
		tokenPutc (token, '\n');
		break;
	case CPP_STRING_SYMBOL:
	case CPP_CHAR_SYMBOL:
		tokenCat (token, cppGetLastCharOrStringContents());
		token->type = TOKEN_ID;
		readId (token);
		break;
	case '\\':
		c = cppGetc ();
		if (c == EOF)
		{
			token->type = TOKEN_EOF;
			break;
		}
		if (c == CPP_STRING_SYMBOL || c == CPP_CHAR_SYMBOL)
		{
			/* broken input */
			tokenCat (token, cppGetLastCharOrStringContents());
		}
		else
			tokenPutc (token, c);
		token->type = TOKEN_ID;
		readId (token);
		break;
	case '=':
		token->type = TOKEN_EQ;
		tokenPutc (token, c);
		break;
	default:
		tokenPutc (token, c);
		token->type = TOKEN_ID;
		readId (token);
		break;
	}

	if (tokenIsType(token, ID))
	{
		token->keyword = lookupKeyword (tokenString (token), Lang_containerfile);
		if (token->keyword != KEYWORD_NONE)
			token->type = TOKEN_KEYWORD;
	}
}

static void skipToNewline (tokenInfo *token)
{
	tokenSkipToType (token, TOKEN_NEWLINE);
}

static int makeImageTag (const char *name, int baseIndex)
{
	tagEntryInfo e;
	tagEntryInfo *be;

	initTagEntry(&e, name, K_IMAEG);

	be = getEntryInCorkQueue(baseIndex);
	if (be != CORK_NIL)
		e.extensionFields.inheritance = be->name;
	return makeTagEntry (&e);
}

static int makeAnonImageTag (int baseIndex)
{
	vString *img = anonGenerateNew ("img", K_IMAEG);
	int r = makeImageTag (vStringValue (img), baseIndex);
	vStringDelete (img);
	return r;
}

static int parseFrom (tokenInfo *token)
{
	int imageIndex = CORK_NIL;

	tokenRead (token);
	if (tokenIsEOF (token))
		return CORK_NIL;
	if (!tokenIsType(token, ID))
		goto out;
	else if (tokenString (token)[0] == '-')
	{
		/* Maybe an option, restart from the next token. */
		return parseFrom(token);
	}

	if (vStringIsEmpty(token->string))
		goto out;

	int baseIndex = makeSimpleRefTag(token->string, K_IMAEG, R_IMAGE_BASE);
	tokenRead (token);

	if (tokenIsEOF (token)
		|| tokenIsType(token, NEWLINE))
		return makeAnonImageTag(baseIndex);

	if (!tokenIsKeyword (token, AS))
	{
		imageIndex = makeAnonImageTag(baseIndex);
		goto out;
	}

	tokenRead (token);
	if (tokenIsEOF (token)
		|| tokenIsType(token, NEWLINE))
		return makeAnonImageTag(baseIndex);

	if ((!tokenIsType(token, ID))
		|| vStringIsEmpty (token->string))
	{
		imageIndex = makeAnonImageTag(baseIndex);
		goto out;
	}

	imageIndex = makeImageTag(tokenString(token), baseIndex);

 out:
	skipToNewline(token);
	return imageIndex;
}

static void makeContainerfileTag (const char *name, int kindIndex, int scope)
{
	tagEntryInfo e;

	initTagEntry(&e, name, kindIndex);

	e.extensionFields.scopeIndex = scope;

	makeTagEntry (&e);
}

static void parseNamedObjects (tokenInfo *token, int kindIndex, int imageIndex)
{
	while (true)
	{
		tokenRead (token);

		if (tokenIsEOF (token)
			|| tokenIsType(token, NEWLINE))
			return;

		if (tokenIsType (token, EQ))
		{
			tokenRead (token);

			/* broken input */
			if (tokenIsEOF (token)
				|| tokenIsType(token, NEWLINE))
				return;
		}
		else if (tokenIsType (token, ID) || tokenIsType (token, KEYWORD))
			makeContainerfileTag (tokenString(token), kindIndex, imageIndex);
		else
		{
			skipToNewline(token);
			return;
		}
	}
}

static void parseEnv (tokenInfo *token, int imageIndex)
{
	while (true)
	{
		tokenRead (token);

		if (tokenIsEOF (token)
			|| tokenIsType(token, NEWLINE))
			return;

		if (tokenIsType (token, EQ))
		{
			tokenRead (token);

			/* broken input */
			if (tokenIsEOF (token)
				|| tokenIsType(token, NEWLINE))
				return;
		}
		else if (tokenIsType (token, ID) || tokenIsType (token, KEYWORD))
		{
			makeContainerfileTag (tokenString(token), K_ENV, imageIndex);

			tokenRead (token);
			if (tokenIsType (token, EQ))
				tokenUnread (token);
		}
		else
		{
			skipToNewline(token);
			return;
		}
	}
}

static void findContainerfileTags (void)
{
	cppInit (false, false, false, false,
			 KIND_GHOST_INDEX, 0, 0,
			 KIND_GHOST_INDEX,
			 KIND_GHOST_INDEX, 0, 0,
			 FIELD_UNKNOWN);

	tokenInfo *const token = newToken (&containerfileTokenInfoClass);
	int imageIndex = CORK_NIL;

	while (true)
	{
		tokenRead (token);
		if (tokenIsEOF (token))
			break;

		if (tokenIsKeyword(token, FROM))
			imageIndex = parseFrom(token);
		else if (tokenIsKeyword(token, ARG))
			parseNamedObjects (token, K_ARG, imageIndex);
		else if (tokenIsKeyword(token, ENV))
			parseEnv (token, imageIndex);
		else if (tokenIsKeyword(token, LABEL))
			parseNamedObjects (token, K_LABEL, imageIndex);
		else
			skipToNewline(token);
	}

	tokenDelete (token);
	cppTerminate ();
}

static void initialize (const langType language)
{
	Lang_containerfile = language;
}

extern parserDefinition* ContainerfileParser (void)
{
	static const char *const patterns [] = { "Containerfile", "Dockerfile", NULL };
	parserDefinition* def = parserNew ("Containerfile");

	def->patterns = patterns;
	def->kindTable = ContainerfileKinds;
	def->kindCount = ARRAY_SIZE (ContainerfileKinds);

	def->initialize = initialize;
	def->parser = findContainerfileTags;
	def->keywordTable = ContainerfileKeywordTable;
	def->keywordCount = ARRAY_SIZE (ContainerfileKeywordTable);

	/* cpreprocessor wants corkQueue. */
	def->useCork    = CORK_QUEUE;

	return def;
}
