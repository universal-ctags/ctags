/*
 *
 *   Copyright (c) 2011, Ivan Krasilnikov
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module implements parsing of protocol buffers definition files
 *   (http://code.google.com/apis/protocolbuffers/docs/proto.html)
 */

/*
  Masatake YAMATO takes this from https://sourceforge.net/p/ctags/patches/74/
  after getting following approval:
  ===============================================================================
  Message-ID: <CALPttHe+hSa_kjwx6GoWS6CsDf_OG0bcmhmPahb4shnKb8tkWg@mail.gmail.com>
  Subject: Re: your protobuf.patch
  From: Ivan Krasilnikov <infnty@gmail.com>
  To: m_yamato@users.sf.net
  Date: Fri, 8 Jul 2016 15:37:07 +0200

  Hi, yes, it's fine, no problem.

  --
  Ivan

  On 8 July 2016 at 06:31, <m_yamato@users.sf.net> wrote:

  > Hi,
  >
  > I am a developer of universal ctags(http://ctags.io).
  >
  > I would like to merge your patch for protobuf in *GPL v2 or later*.
  >
  > Is it o.k.?
  > ------------------------------
  >
  > This message was sent to you via the SourceForge web mail form.
  > You may reply to this message directly, or at
  > https://sourceforge.net/u/userid-2121776/profile/send_message
  >
  ===============================================================================
*/

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>

#include "cpreprocessor.h"

#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "vstring.h"

/*
 *   DATA DEFINITIONS
 */
static langType Lang_protobuf;

typedef enum {
	PK_PACKAGE,
	PK_MESSAGE,
	PK_FIELD,
	PK_ENUMERATOR,
	PK_ENUM,
	PK_SERVICE,
	PK_RPC
} protobufKind;

typedef enum {
	R_MESSAGE_EXTENSION,
} protobufMessageRole;

static roleDefinition ProtobufMessageRoles [] = {
	{ true, "extension", "extending the message" },
};

static kindDefinition ProtobufKinds [] = {
	{ true,  'p', "package",    "packages" },
	{ true,  'm', "message",    "messages",
	  .referenceOnly = false, ATTACH_ROLES (ProtobufMessageRoles)},
	{ true,  'f', "field",      "fields" },
	{ true,  'e', "enumerator", "enum constants" },
	{ true,  'g', "enum",       "enum types" },
	{ true,  's', "service",    "services" },
	{ true,  'r', "rpc",        "RPC methods" },
};

typedef enum eKeywordId {
	KEYWORD_OPTION,
	KEYWORD_PACKAGE,
	KEYWORD_MESSAGE,
	KEYWORD_ENUM,
	KEYWORD_REPEATED,
	KEYWORD_OPTIONAL,
	KEYWORD_REQUIRED,
	KEYWORD_SERVICE,
	KEYWORD_RPC,
	KEYWORD_STREAM,
	KEYWORD_RETURNS,
	KEYWORD_EXTEND,
} keywordId;

static const keywordTable ProtobufKeywordTable [] = {
	{ "option",   KEYWORD_OPTION   },
	{ "package",  KEYWORD_PACKAGE  },
	{ "message",  KEYWORD_MESSAGE  },
	{ "enum",     KEYWORD_ENUM     },
	{ "repeated", KEYWORD_REPEATED },
	{ "optional", KEYWORD_OPTIONAL },
	{ "required", KEYWORD_REQUIRED },
	{ "service",  KEYWORD_SERVICE  },
	{ "rpc",      KEYWORD_RPC      },
	{ "stream",   KEYWORD_STREAM   },
	{ "returns",  KEYWORD_RETURNS  },
	{ "extend",   KEYWORD_EXTEND   },
};

#define TOKEN_EOF   0
#define TOKEN_ID    'i'

static struct sTokenInfo {
	int type;         /* one of TOKEN_* constants or punctuation characters */
	keywordId keyword;
	vString *value;
} token;

/*
 *   FUNCTION DEFINITIONS
 */

static void nextToken (void)
{
	int c;

repeat:
	/*
	 * .proto files may contain C and C++ style comments and
	 * quoted strings. cppGetc() takes care of them.
	 */
	c = cppGetc ();

	token.keyword = KEYWORD_NONE;
	if (c <= 0)
		token.type = TOKEN_EOF;
	else if (c == '{' || c == '}' || c == ';' || c == '.' || c == '=')
		token.type = c;
	else if (cppIsalnum (c) || c == '_')
	{
		token.type = TOKEN_ID;
		vStringClear (token.value);
		while (c > 0 && (cppIsalnum (c) || c == '_')) {
			vStringPut (token.value, c);
			c = cppGetc ();
		}
		token.keyword = lookupCaseKeyword (vStringValue (token.value), Lang_protobuf);
		cppUngetc (c);
	}
	else
		goto repeat;  /* anything else is not important for this parser */
}

static void skipUntil (const char *punctuation)
{
	while (token.type != TOKEN_EOF && strchr (punctuation, token.type) == NULL)
		nextToken ();
}

static int tokenIsKeyword(keywordId keyword)
{
	return token.type == TOKEN_ID && token.keyword == keyword;
}

static int createProtobufTagFull (const vString *name, int kind, int role, int scopeCorkIndex)
{
	static tagEntryInfo tag;
	int corkIndex = CORK_NIL;

	if (ProtobufKinds [kind].enabled)
	{
		initRefTagEntry (&tag, vStringValue (name), kind, role);
		tag.extensionFields.scopeIndex = scopeCorkIndex;
		corkIndex = makeTagEntry (&tag);
	}

	return corkIndex;
}

static int createProtobufTag (const vString *name, int kind, int scopeCorkIndex)
{
	return createProtobufTagFull (name, kind, ROLE_DEFINITION_INDEX, scopeCorkIndex);
}

static void parseEnumConstants (int scopeCorkIndex)
{
	if (token.type != '{')
		return;
	nextToken ();

	while (token.type != TOKEN_EOF && token.type != '}')
	{
		if (token.type == TOKEN_ID && !tokenIsKeyword (KEYWORD_OPTION))
		{
			nextToken ();  /* doesn't clear token.value if it's punctuation */
			if (token.type == '=')
				createProtobufTag (token.value, PK_ENUMERATOR, scopeCorkIndex);
		}

		skipUntil (";}");

		if (token.type == ';')
			nextToken ();
	}
}

#define gatherTypeinfo(VSTRING,CONDITION)			\
	while (CONDITION)								\
	{												\
		if (token.type == TOKEN_ID)					\
			vStringCat (VSTRING, token.value);		\
		else if (tokenIsKeyword (KEYWORD_STREAM))	\
		{											\
			vStringCat (VSTRING, token.value);		\
			vStringPut (VSTRING, ' ');				\
		}											\
		else										\
			vStringPut (VSTRING, token.type);		\
		nextToken ();								\
	}

static void parseRPCTypeinfos (int corkIndex)
{
	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);

	vString *signature = vStringNew ();
	gatherTypeinfo(signature,
				   (token.type != TOKEN_EOF
					&& token.type != '{' && token.type != ';'
					&& !tokenIsKeyword (KEYWORD_RETURNS)));
	if (!vStringIsEmpty(signature))
		e->extensionFields.signature = vStringDeleteUnwrap (signature);
	else
		vStringDelete (signature);

	if (!tokenIsKeyword (KEYWORD_RETURNS))
		return;
	nextToken ();

	vString *typeref = vStringNew ();
	gatherTypeinfo(typeref, (token.type != EOF
							 && token.type != '{' && token.type != ';'));
	if (!vStringIsEmpty(typeref))
	{
		e->extensionFields.typeRef [0] = eStrdup ("typename"); /* As C++ parser does */
		e->extensionFields.typeRef [1] = vStringDeleteUnwrap (typeref);
	}
	else
		vStringDelete (typeref);
}

static int parseStatementFull (int kind, int role, int scopeCorkIndex)
{
	vString *fullName = NULL;

	nextToken ();

	if (kind == PK_FIELD)
	{
		/* skip field's type */
		do
		{
			if (token.type == '.')
				nextToken ();
			if (token.type != TOKEN_ID)
				return CORK_NIL;
			nextToken ();
		} while (token.type == '.');
	}

	/* When extending message defined in the external package, the name
	 * becomes longer. */
	if (kind == PK_MESSAGE && role == R_MESSAGE_EXTENSION)
	{
		if (token.type != TOKEN_ID)
			return CORK_NIL;

		fullName = vStringNewCopy (token.value);
		while (true)
		{
			nextToken ();

			if (token.type == TOKEN_ID)
				vStringCat (fullName, token.value);
			else if (token.type == '.')
				vStringPut (fullName, '.');
			else
				break;
		}
	}
	else if (token.type != TOKEN_ID)
		return CORK_NIL;

	int corkIndex = createProtobufTagFull (fullName? fullName: token.value,
										   kind, role, scopeCorkIndex);

	if (!fullName)
		nextToken ();
	vStringDelete (fullName);

	if (kind == PK_RPC && corkIndex != CORK_NIL)
		parseRPCTypeinfos (corkIndex);

	if (kind == PK_ENUM)
		parseEnumConstants (corkIndex);

	return corkIndex;
}

static int parseStatement (int kind, int scopeCorkIndex)
{
	return parseStatementFull (kind, ROLE_DEFINITION_INDEX, scopeCorkIndex);
}

static int parsePackage (void)
{
	int corkIndex = CORK_NIL;

	vString *pkg = vStringNew ();

	while (true)
	{
		nextToken ();

		if (token.type == TOKEN_ID)
			vStringCat (pkg, token.value);
		else if (token.type == '.')
			vStringPut (pkg, '.');
		else
			break;
	}

	if (vStringLength (pkg) > 0)
		corkIndex = createProtobufTag (pkg, PK_PACKAGE, CORK_NIL);
	vStringDelete (pkg);
	return corkIndex;
}

static void findProtobufTags (void)
{
	cppInit (false, false, false, false,
			 KIND_GHOST_INDEX, 0, KIND_GHOST_INDEX,
			 KIND_GHOST_INDEX, 0, 0,
			 FIELD_UNKNOWN);
	token.value = vStringNew ();

	nextToken ();

	int scopeCorkIndex = CORK_NIL;
	while (token.type != TOKEN_EOF)
	{
		int corkIndex = CORK_NIL;
		bool dontChangeScope = false;
		if (tokenIsKeyword (KEYWORD_PACKAGE))
		{
			corkIndex = parsePackage ();
			scopeCorkIndex = corkIndex;
		}
		else if (tokenIsKeyword (KEYWORD_MESSAGE))
			corkIndex = parseStatement (PK_MESSAGE, scopeCorkIndex);
		else if (tokenIsKeyword (KEYWORD_ENUM))
		{
			corkIndex = parseStatement (PK_ENUM, scopeCorkIndex);
			dontChangeScope = true;
		}
		else if (tokenIsKeyword (KEYWORD_REPEATED) || tokenIsKeyword (KEYWORD_OPTIONAL) || tokenIsKeyword (KEYWORD_REQUIRED))
			corkIndex = parseStatement (PK_FIELD, scopeCorkIndex);
		else if (tokenIsKeyword (KEYWORD_SERVICE))
			corkIndex = parseStatement (PK_SERVICE, scopeCorkIndex);
		else if (tokenIsKeyword (KEYWORD_RPC))
			corkIndex = parseStatement (PK_RPC, scopeCorkIndex);
		else if (tokenIsKeyword (KEYWORD_EXTEND))
			corkIndex = parseStatementFull (PK_MESSAGE, R_MESSAGE_EXTENSION, scopeCorkIndex);

		skipUntil (";{}");
		if (!dontChangeScope && token.type == '{' && corkIndex != CORK_NIL)
		{
			/* Enter the new scope. */
			scopeCorkIndex = corkIndex;
		}
		else if (!dontChangeScope && token.type == '}' && scopeCorkIndex != CORK_NIL)
		{
			/* Return to the parent scope. */
			tagEntryInfo *e = getEntryInCorkQueue (scopeCorkIndex);
			scopeCorkIndex = e->extensionFields.scopeIndex;
		}
		nextToken ();
	}

	vStringDelete (token.value);
	cppTerminate ();
}

static void initialize (const langType language)
{
	Lang_protobuf = language;
}

extern parserDefinition* ProtobufParser (void)
{
	static const char *const extensions [] = { "proto", NULL };
	parserDefinition* def = parserNew ("Protobuf");

	def->extensions = extensions;
	def->kindTable      = ProtobufKinds;
	def->initialize = initialize;
	def->kindCount  = ARRAY_SIZE (ProtobufKinds);
	def->parser     = findProtobufTags;
	def->keywordTable = ProtobufKeywordTable;
	def->keywordCount = ARRAY_SIZE (ProtobufKeywordTable);

	/* cpreprocessor wants corkQueue. */
	def->useCork    = CORK_QUEUE;

	return def;
}
