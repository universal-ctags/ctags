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

#include "lcpp.h"

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

static kindOption ProtobufKinds [] = {
	{ TRUE,  'p', "package",    "packages" },
	{ TRUE,  'm', "message",    "messages" },
	{ TRUE,  'f', "field",      "fields" },
	{ TRUE,  'e', "enumerator", "enum constants" },
	{ TRUE,  'g', "enum",       "enum types" },
	{ TRUE,  's', "service",    "services" },
	{ FALSE, 'r', "rpc",        "RPC methods" }
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
	else if (isalnum (c) || c == '_')
	{
		token.type = TOKEN_ID;
		vStringClear (token.value);
		while (c > 0 && (isalnum (c) || c == '_')) {
			vStringPut (token.value, c);
			c = cppGetc ();
		}
		token.keyword = analyzeToken (token.value, Lang_protobuf);
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

static void createProtobufTag (const vString *name, int kind)
{
	static tagEntryInfo tag;

	if (ProtobufKinds [kind].enabled)
	{
		initTagEntry (&tag, vStringValue (name), ProtobufKinds +kind);
		makeTagEntry (&tag);
	}
}

static void parseEnumConstants (void)
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
				createProtobufTag (token.value, PK_ENUMERATOR);
		}

		skipUntil (";}");

		if (token.type == ';')
			nextToken ();
	}
}

static void parseStatement (int kind)
{
	nextToken ();

	if (kind == PK_FIELD)
	{
		/* skip field's type */
		do
		{
			if (token.type == '.')
				nextToken ();
			if (token.type != TOKEN_ID)
				return;
			nextToken ();
		} while (token.type == '.');
	}

	if (token.type != TOKEN_ID)
		return;

	createProtobufTag (token.value, kind);
	nextToken ();

	if (kind == PK_ENUM)
		parseEnumConstants ();
}

static void findProtobufTags (void)
{
	cppInit (FALSE, FALSE, FALSE, FALSE, NULL,
		 ROLE_INDEX_DEFINITION, NULL,
		 ROLE_INDEX_DEFINITION, ROLE_INDEX_DEFINITION);
	token.value = vStringNew ();

	nextToken ();

	while (token.type != TOKEN_EOF)
	{
		if (tokenIsKeyword (KEYWORD_PACKAGE))
			parseStatement (PK_PACKAGE);
		else if (tokenIsKeyword (KEYWORD_MESSAGE))
			parseStatement (PK_MESSAGE);
		else if (tokenIsKeyword (KEYWORD_ENUM))
			parseStatement (PK_ENUM);
		else if (tokenIsKeyword (KEYWORD_REPEATED) || tokenIsKeyword (KEYWORD_OPTIONAL) || tokenIsKeyword (KEYWORD_REQUIRED))
			parseStatement (PK_FIELD);
		else if (tokenIsKeyword (KEYWORD_SERVICE))
			parseStatement (PK_SERVICE);
		else if (tokenIsKeyword (KEYWORD_RPC))
			parseStatement (PK_RPC);

		skipUntil (";{}");
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
	def->kinds      = ProtobufKinds;
	def->initialize = initialize;
	def->kindCount  = ARRAY_SIZE (ProtobufKinds);
	def->parser     = findProtobufTags;
	def->keywordTable = ProtobufKeywordTable;
	def->keywordCount = ARRAY_SIZE (ProtobufKeywordTable);

	return def;
}
