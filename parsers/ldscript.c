/*
 *   Copyright (c) 2016, Masatake YAMATO
 *   Copyright (c) 2016, Red Hat, Inc.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for GNU linker script
 *   files.
 */

#include "general.h"
#include "tokeninfo.h"

#include "entry.h"
#include "cpreprocessor.h"
#include "keyword.h"
#include "parse.h"
#include "ptrarray.h"
#include "read.h"
#include "trace.h"
#include "xtag.h"

#include <string.h>

/*
 *   DATA DEFINITIONS
 */

typedef enum {
	LD_SCRIPT_SYMBOL_ENTRYPOINT,
	LD_SCRIPT_SYMBOL_ALIASED,
} ldScriptSymbolRole;

static roleDefinition LdScriptSymbolRoles [] = {
	{ true, "entrypoint", "entry points" },
	{ true, "aliased", "aliased with __attribute__((alias(...))) in C/C++ code" },
};

typedef enum {
	LD_SCRIPT_INPUT_SECTION_MAPPED,
	LD_SCRIPT_INPUT_SECTION_DISCARDED,
	LD_SCRIPT_INPUT_SECTION_DESTINATION,
} ldScriptInputSectionRole;

static roleDefinition LdScriptInputSectionRoles [] = {
	{ true, "mapped",  "mapped to output section" },
	{ true, "discarded", "discarded when linking" },
	{ true, "destination", "specified as the destination of code and data" },
};

typedef enum {
	K_SECTION,
	K_SYMBOL,
	K_VERSION,
	K_INPUT_SECTION,
} ldScriptKind;

static kindDefinition LdScriptKinds [] = {
	{ true, 'S', "section", "sections" },
	{ true, 's', "symbol",  "symbols",
	  .referenceOnly = false, ATTACH_ROLES(LdScriptSymbolRoles)},
	{ true, 'v', "version", "versions" },
	{ true, 'i', "inputSection", "input sections",
	  .referenceOnly = false, ATTACH_ROLES(LdScriptInputSectionRoles)},
};

enum {
	KEYWORD_ENTRY,
	KEYWORD_SECTIONS,
	KEYWORD_LOC,
	KEYWORD_AT,
	KEYWORD_VERSION,
	KEYWORD_PROVIDE,
	KEYWORD_PROVIDE_HIDDEN,
	KEYWORD_HIDDEN,
	KEYWORD_EXCLUDE_FILE,
	KEYWORD_INPUT_SECTION_FLAGS,
	KEYWORD_COMMON,
	KEYWORD_KEEP,
	KEYWORD_DATA,
};
typedef int keywordId; /* to allow KEYWORD_NONE */


static const keywordTable LdScriptKeywordTable[] = {
	/* keyword			keyword ID */
	{ "ENTRY",			KEYWORD_ENTRY			},
	{ "SECTIONS",		KEYWORD_SECTIONS		},
	{ ".",				KEYWORD_LOC				},
	{ "AT",				KEYWORD_AT				},
	{ "VERSION",		KEYWORD_VERSION			},
	{ "PROVIDE",		KEYWORD_PROVIDE			},
	{ "PROVIDE_HIDDEN",	KEYWORD_PROVIDE_HIDDEN	},
	{ "HIDDEN",	        KEYWORD_HIDDEN	        },
	{ "EXCLUDE_FILE",   KEYWORD_EXCLUDE_FILE    },
	{ "INPUT_SECTION_FLAGS", KEYWORD_INPUT_SECTION_FLAGS },
	{ "COMMON",			KEYWORD_COMMON },
	{ "KEEP",			KEYWORD_KEEP },
	{ "SORT",			KEYWORD_KEEP },
	{ "BYTE",			KEYWORD_DATA },
	{ "SHORT",			KEYWORD_DATA },
	{ "LONG",			KEYWORD_DATA },
	{ "QUAD",			KEYWORD_DATA },
	{ "SQUAD",			KEYWORD_DATA },
	{ "FILL",			KEYWORD_DATA },
};

enum eTokenType {
	/* 0..255 are the byte's value */
	TOKEN_EOF = 256,
	TOKEN_UNDEFINED,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
	TOKEN_NUMBER,
	TOKEN_ASSIGNMENT_OP,
	TOKEN_OP,
	TOKEN_PHDIR,
	TOKEN_REGION,
	TOKEN_FILLEXP,
	TOKEN_DISCARD,
};

static void readToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED);
static void clearToken (tokenInfo *token);
static void copyToken (tokenInfo *dest, tokenInfo *src, void *data CTAGS_ATTR_UNUSED);

typedef struct sLdScriptToken {
	tokenInfo base;
	int scopeIndex;
	tokenKeyword assignment;
	bool whitespacePrefixed;
} ldScriptToken;

#define LDSCRIPT(TOKEN) ((ldScriptToken *)TOKEN)

static struct tokenTypePair ldScriptTypePairs [] = {
	{ '{', '}' },
};

static struct tokenInfoClass ldScriptTokenInfoClass = {
	.nPreAlloc = 4,
	.typeForUndefined = TOKEN_UNDEFINED,
	.keywordNone      = KEYWORD_NONE,
	.typeForKeyword   = TOKEN_KEYWORD,
	.typeForEOF       = TOKEN_EOF,
	.extraSpace       = sizeof (ldScriptToken) - sizeof (tokenInfo),
	.pairs            = ldScriptTypePairs,
	.pairCount        = ARRAY_SIZE (ldScriptTypePairs),
	.read             = readToken,
	.clear            = clearToken,
	.copy             = copyToken,
};

typedef enum {
	F_ASSIGNMENT,
	COUNT_FIELD
} ldScriptField;

static fieldDefinition LdScriptFields[COUNT_FIELD] = {
	{ .name = "assignment",
	  .description = "how a value is assigned to the symbol",
	  .enabled = true },
};

static langType Lang_ldscript;

/*
 *   FUNCTION DEFINITIONS
 */

static tokenInfo *newLdScriptToken (void)
{
	return newToken (&ldScriptTokenInfoClass);
}

static void clearToken (tokenInfo *token)
{
	LDSCRIPT(token)->scopeIndex = CORK_NIL;
	LDSCRIPT(token)->assignment = KEYWORD_NONE;
}

static void copyToken (tokenInfo *dest, tokenInfo *src, void *data CTAGS_ATTR_UNUSED)
{
	LDSCRIPT (dest)->scopeIndex =
		LDSCRIPT (src)->scopeIndex;
	LDSCRIPT (dest)->assignment =
		LDSCRIPT (src)->assignment;
	LDSCRIPT (dest)->whitespacePrefixed =
		LDSCRIPT (src)->whitespacePrefixed;
}

static int makeLdScriptTagMaybe (tagEntryInfo *const e, tokenInfo *const token,
								 int kind, int role)
{
	if (role == ROLE_DEFINITION_INDEX)
	{
		if (! LdScriptKinds[kind].enabled)
			return CORK_NIL;
	}
	else if (! (isXtagEnabled (XTAG_REFERENCE_TAGS)
				&& LdScriptKinds[kind].roles[role].enabled))
		return CORK_NIL;

	initRefTagEntry (e, tokenString (token),
					 kind,
					 role);
	updateTagLine (e, token->lineNumber, token->filePosition);
	e->extensionFields.scopeIndex = LDSCRIPT (token)->scopeIndex;

	/* TODO: implement file: field. */
	if ((kind == K_SYMBOL)
		&& LdScriptFields[F_ASSIGNMENT].enabled)
	{
		const char *assignment = NULL;

		switch (LDSCRIPT (token)->assignment)
		{
		case KEYWORD_PROVIDE:
			assignment = "provide";
			break;
		case KEYWORD_PROVIDE_HIDDEN:
			assignment = "provide_hidden";
			break;
		case KEYWORD_HIDDEN:
			assignment = "hidden";
			break;
		}

		if (assignment)
			attachParserField (e, LdScriptFields[F_ASSIGNMENT].ftype,
							   assignment);
	}

	return makeTagEntry (e);
}

#define isIdentifierChar(c)										\
	(cppIsalnum (c) || (c) == '_' || (c) == '.' || (c) == '-' \
	 || (((c) >= 0x80) && ((c) <= 0xff)))

static int readPrefixedToken (tokenInfo *const token, int type)
{
	int n = 0;
	int c;

	while ((c = cppGetc()) != EOF)
	{
		if (isIdentifierChar (c))
		{
			n++;
			tokenPutc (token, c);
		}
		else
		{
			cppUngetc (c);
			break;
		}
	}

	if (n)
		token->type = type;
	return n;
}

// We stop applying macro replacements if a macro is used so many
// times in a recursive macro expansion.
#define LD_SCRIPT_PARSER_MAXIMUM_MACRO_USE_COUNT 8

static bool collectMacroArguments (ptrArray *args)
{
	vString *s = vStringNew ();
	tokenInfo *const t = newLdScriptToken ();
	int depth = 1;

	do
	{
		tokenRead (t);

		if (tokenIsType (t, EOF))
			break;
		else if (tokenIsTypeVal (t, ')'))
		{
			depth--;
			if (depth == 0)
			{
				char *cstr = vStringDeleteUnwrap (s);
				ptrArrayAdd (args, cstr);
				s = NULL;
			}
			else
				vStringCat (s, t->string);
		}
		else if (tokenIsTypeVal (t, '('))
		{
			depth++;
			vStringCat (s, t->string);
		}
		else if (tokenIsTypeVal (t, ','))
		{
			char *cstr = vStringDeleteUnwrap (s);
			ptrArrayAdd (args, cstr);
			s = vStringNew ();
		}
		else
		{
			if (LDSCRIPT(t)->whitespacePrefixed)
				vStringPut(s, ' ');
			vStringCat (s, t->string);
		}
	}
	while (depth > 0);

	vStringDelete (s);			/* NULL is acceptable. */

	tokenDelete (t);

	if (depth > 0)
		TRACE_PRINT("unbalanced argument list");

	return (depth > 0)? false: true;
}

static bool expandCppMacro (cppMacroInfo *macroInfo)
{
	ptrArray *args = NULL;

	if (macroInfo->hasParameterList)
	{
		tokenInfo *const t = newLdScriptToken ();

		/* Though macro arguments are expected, they are not found. */
		tokenRead (t);
		if (!tokenIsTypeVal (t, '('))
		{
			tokenUnread (t);
			tokenDelete (t);
			TRACE_PRINT("no argument for the %s<%p>", macroInfo->name, macroInfo);
			return false;
		}

		args = ptrArrayNew (eFree);
		if (!collectMacroArguments (args))
		{
			ptrArrayDelete (args);
			tokenUnread (t);
			tokenDelete (t);
			return false;
		}
		tokenDelete (t);
	}

#ifdef DO_TRACING
	if (args)
	{
		for (int i = 0; i < ptrArrayCount(args); i++)
			TRACE_PRINT("[%d] %s", i, (const char *)ptrArrayItem (args, i));
	}
#endif

	cppBuildMacroReplacementWithPtrArrayAndUngetResult (macroInfo, args);

	ptrArrayDelete (args);		/* NULL is acceptable. */
	return true;
}

static void readToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED)
{
	int c, c0, c1;

	token->type		= TOKEN_UNDEFINED;
	token->keyword	= KEYWORD_NONE;
	vStringClear (token->string);

	int prefix_count = -1;
	LDSCRIPT (token)->whitespacePrefixed = false;
	do {
		c = cppGetc();
		prefix_count++;
		if (prefix_count > 0)
			LDSCRIPT (token)->whitespacePrefixed = true;
	} while (c == ' ' || c== '\t' || c == '\f' || c == '\r' || c == '\n'
			 || c == STRING_SYMBOL || c == CHAR_SYMBOL);

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	switch (c)
	{
	case EOF:
		token->type = TOKEN_EOF;
		break;
	case ';':
	case '(':
	case ')':
	case '{':
	case '}':
	case '[':
	case ']':
		tokenPutc(token, c);
		token->type = c;
		break;
	case '~':
	case '%':
	case '?':
		tokenPutc(token, c);
		token->type = TOKEN_OP;
		break;
	case '-':
	case '+':
	case '*':
	case '/':					/* -,+,*,/,-=,+=,*=,/= */
		tokenPutc(token, c);
		c0 = cppGetc ();
		token->type = TOKEN_OP;
		if (c0 == '=')
		{
			tokenPutc(token, c0);
			token->type = TOKEN_ASSIGNMENT_OP;
		}
		else if (c == '/' && c0 == 'D')
		{
			tokenInfo *const discard = newLdScriptToken ();

			cppUngetc (c0);
			tokenRead (discard);
			if (tokenIsType(discard, IDENTIFIER) &&
				(strcmp(tokenString(discard), "DISCARD")) == 0)
			{
				c1 = cppGetc ();
				if (c1 == '/')
					token->type = TOKEN_DISCARD;
				else
				{
					cppUngetc (c1);
					tokenUnread (discard);
				}
			}
			else
				tokenUnread (discard);
			tokenDelete (discard);
		}
		else
			cppUngetc (c0);
		break;
	case '!':					/* !, != */
		tokenPutc(token, c);
		token->type = TOKEN_OP;
		c0 = cppGetc ();
		if (c0 == '=')
			tokenPutc(token, c0);
		else
			cppUngetc (c0);
	case '<':					/* <,<=,<<,<<= */
		tokenPutc(token, c);
		token->type = TOKEN_OP;
		c0 = cppGetc ();
		if (c0 == c  || c0 == '=')
		{
			tokenPutc(token, c0);
			if (c0 == c)
			{
				c1 = cppGetc ();
				if (c1 == '=')
				{
					tokenPutc(token, c1);
					token->type = TOKEN_ASSIGNMENT_OP;
				}
				else
					cppUngetc (c1);
			}
		}
		else
			cppUngetc (c0);
	case '|':					/* |,||,|= */
	case '&':					/* &,&&,&= */
		tokenPutc(token, c);
		token->type = TOKEN_OP;
		c0 = cppGetc ();
		if (c0 == c)
			tokenPutc(token, c0);
		else if (c0 == '=')
		{
			tokenPutc(token, c0);
			token->type = TOKEN_ASSIGNMENT_OP;
		}
		else
			cppUngetc (c0);
		break;
	case '=':					/* =,== */
		tokenPutc(token, c);
		if (!readPrefixedToken (token, TOKEN_FILLEXP))
		{
			c0 = cppGetc ();
			if (c0 == '=')
			{
				tokenPutc(token, c0);
				token->type = TOKEN_OP;
			}
			else
			{
				cppUngetc (c0);
				token->type = TOKEN_ASSIGNMENT_OP;
			}
		}
		break;
	case '>':					/* >,>>,>>= */
		tokenPutc(token, c);
		if (!readPrefixedToken (token, TOKEN_REGION))
		{
			token->type = TOKEN_OP;
			c0 = cppGetc ();
			if (c0 == c  || c0 == '=')
			{
				tokenPutc(token, c0);
				c1 = cppGetc();
				if (c1 == '=')
				{
					tokenPutc(token, c1);
					token->type = TOKEN_ASSIGNMENT_OP;
				}
				else
					cppUngetc (c1);
			}
			else
				cppUngetc (c0);
		}
		break;
	case ':':
		tokenPutc(token, c);
		if (!readPrefixedToken (token, TOKEN_PHDIR))
			token->type = c;
		break;
	default:
		if (cppIsdigit (c))
		{
			/* Using cppIsdigit here is redundant.
			 *
			 * `c' never takes STRING_SYMBOL or CHAR_SYMBOL as
			 * its value here.
			 * However, the practice using cppIs... macros for the value
			 * returned from cppGetc() may avoid unexpected programming
			 * mistakes I took repeatedly.
			 */
			token->type = TOKEN_NUMBER;
			tokenPutc(token, c);
			while ((c = cppGetc()))
			{
				if (isIdentifierChar (c))
					tokenPutc(token, c);
				else
				{
					cppUngetc (c);
					break;
				}
			}
		}
		else if (isIdentifierChar(c))
		{
			tokenPutc(token, c);
			while ((c = cppGetc()))
			{
				if (isIdentifierChar(c))
					tokenPutc(token, c);
				else
				{
					cppUngetc (c);
					break;
				}
			}
			token->keyword = lookupKeyword (vStringValue (token->string), Lang_ldscript);
			if (token->keyword == KEYWORD_NONE)
			{
				token->type = TOKEN_IDENTIFIER;

				cppMacroInfo *macroInfo = cppFindMacro (vStringValue (token->string));
				if (macroInfo)
				{
					TRACE_PRINT("Macro expansion: %s<%p>%s", vStringValue (token->string),
								macroInfo, macroInfo->hasParameterList? "(...)": "");
					if (!(macroInfo->useCount < LD_SCRIPT_PARSER_MAXIMUM_MACRO_USE_COUNT))
						TRACE_PRINT ("Overly uesd macro %s<%p> useCount: %d (> %d)",
									 vStringValue (token->string), macroInfo, macroInfo->useCount,
									 LD_SCRIPT_PARSER_MAXIMUM_MACRO_USE_COUNT);
					else if (expandCppMacro (macroInfo))
						readToken (token, NULL);
				}
			}
			else
				token->type = TOKEN_KEYWORD;
		}
		else
		{
			tokenPutc(token, c);
			token->type = c;
		}
		break;
	}
}

static void parseEntry (tokenInfo *const token)
{
	tokenRead (token);
	if (token->type == '(')
	{
		tokenInfo *const name = newLdScriptToken ();

		tokenRead (name);
		if (tokenIsType(name, IDENTIFIER))
		{
			tagEntryInfo e;

			makeLdScriptTagMaybe (&e, name, K_SYMBOL, LD_SCRIPT_SYMBOL_ENTRYPOINT);
			tokenRead (token);
			tokenSkipToType (token, ')');
		}
		tokenDelete (name);
	}
}

static void parseProvide (tokenInfo * token)
{
	tokenKeyword p = token->keyword;

	if (tokenSkipToType (token, '('))
	{
		tagEntryInfo e;
		tokenRead (token);
		if (tokenIsType(token, IDENTIFIER))
		{
			LDSCRIPT (token)->assignment = p;

			makeLdScriptTagMaybe (&e, token,
								  K_SYMBOL, ROLE_DEFINITION_INDEX);
			LDSCRIPT (token)->assignment = KEYWORD_NONE;
		}
		tokenSkipToType (token, ')');
	}
}

/* An example of input sections:

	(.text .rdata)

  .text and .rtada are input sections. */
static void parseInputSections (tokenInfo *const token)
{
	tagEntryInfo e;
	do {
		tokenRead (token);
		if (token->type == ')')
			break;
		else if (tokenIsType (token, IDENTIFIER))
			makeLdScriptTagMaybe (&e, token,
								  K_INPUT_SECTION,
								  LDSCRIPT(token)->scopeIndex == CORK_NIL
								  ? LD_SCRIPT_INPUT_SECTION_DISCARDED
								  : LD_SCRIPT_INPUT_SECTION_MAPPED);
		else if (tokenIsKeyword (token, EXCLUDE_FILE))
			tokenSkipToType (token, ')');
	} while (!tokenIsEOF (token));
}

/* Symbols and input sections are captured here.
   An example of output sections:

		__brk_base = .;
		. += 64 * 1024;
		*(.brk_reservation)
		__brk_limit = .;

  __brk_base and __brk_limit should be recorded as a symbol.
  .brk_reservationshould be recorded as an input section. */

static void parseOutputSectionCommands (tokenInfo *const token, int terminator)
{
	tokenInfo *const tmp = newLdScriptToken ();
	int scope_index = LDSCRIPT (token)->scopeIndex;

	do {
		tokenRead (token);
		LDSCRIPT (token)->scopeIndex = scope_index;

		if (tokenIsKeyword (token, INPUT_SECTION_FLAGS))
		{
			tokenSkipToType (token, '(');
			tokenSkipToType (token, ')');
		}
		else if (tokenIsKeyword (token, KEEP))
		{
			tokenSkipToType (token, '(');
			parseOutputSectionCommands (token, ')');
		}
		else if (tokenIsType (token,IDENTIFIER)
				 || tokenIsKeyword (token, LOC))
		{
			tagEntryInfo e;

			tokenRead (tmp);
			if (tokenIsType (tmp, ASSIGNMENT_OP))
			{
				if (! tokenIsKeyword (token, LOC))
					makeLdScriptTagMaybe (&e, token,
										  K_SYMBOL, ROLE_DEFINITION_INDEX);
				tokenSkipToType (token, ';');
			}
			else if (tmp->type == '(')
				parseInputSections (token);
			else
				tokenUnread (tmp);
		}
		/* `*',`?', `[', `]' can be part of a pattern of input object file names
		   as a meta character.
		   `[' can enumerated here because it cannot be at the end of pattern. */
		else if ((token->type == TOKEN_OP
				  && ((tokenString(token)[0] == '*')
					  || (tokenString(token)[0] == '?'))
				  && (tokenString(token)[1] == '\0'))
				 || token->type == ']')
		{
			tokenRead (tmp);
			if (tmp->type == '(')
				parseInputSections (token);
			else
				tokenUnread (tmp);
		}
		else if (tokenIsKeyword (token, PROVIDE)
				 || tokenIsKeyword (token, PROVIDE_HIDDEN)
				 || tokenIsKeyword (token, HIDDEN))
			parseProvide (token);
	} while (! (tokenIsEOF (token) || token->type == terminator));

	tokenDelete (tmp);
}

static void parseSection (tokenInfo * name)
{
	tokenInfo *const token = newLdScriptToken ();
	tagEntryInfo e;

	tokenRead (token);

	if (tokenIsType (token, ASSIGNMENT_OP))
	{
		if (!tokenIsKeyword (name, LOC))
			makeLdScriptTagMaybe (&e, name,
								  K_SYMBOL, ROLE_DEFINITION_INDEX);
		tokenSkipToType (token, ';');
	}
	else
	{
	retry:
		if (tokenIsType (token, NUMBER))
			tokenSkipToType (token, ':');
		else if (tokenIsType (token, IDENTIFIER))
		{
			tokenCopy (name, token);
			tokenRead (token);
			goto retry;
		}
		else if (token->type == '(')
			tokenSkipToType (token, ')');

		if (token->type == ':')
		{
			int scope_index;

			if (tokenIsType (name, DISCARD))
				scope_index = CORK_NIL;
			else
				scope_index = makeLdScriptTagMaybe (&e, name,
													K_SECTION, ROLE_DEFINITION_INDEX);

			if (tokenSkipToType (token, '{'))
			{
				LDSCRIPT (token)->scopeIndex = scope_index;
				parseOutputSectionCommands (token, '}');
			}
		}
	}
	tokenDelete (token);
}

static void parseSections (tokenInfo *const token)
{
	tokenRead (token);
	if (token->type == '{')
	{
		do {
			tokenRead (token);
			if (tokenIsKeyword (token, ENTRY))
				parseEntry (token);
			else if (tokenIsType(token, IDENTIFIER)
					 || tokenIsKeyword (token, LOC)
					 || tokenIsType (token, DISCARD))
				parseSection (token);
			else if (tokenIsKeyword (token, PROVIDE)
					 || tokenIsKeyword (token, PROVIDE_HIDDEN)
					 || tokenIsKeyword (token, HIDDEN))
				parseProvide (token);
		} while (! (tokenIsEOF (token) || token->type == '}'));
	}
}

static void parseVersion (tokenInfo *const token)
{
	tagEntryInfo e;
	makeLdScriptTagMaybe (&e, token,
						  K_VERSION, ROLE_DEFINITION_INDEX);

	if (tokenSkipToType (token, '{'))
		tokenSkipOverPair (token);
}

static void parseVersions (tokenInfo *const token)
{
	tokenRead (token);
	if (token->type == '{')
	{
		tokenInfo *curly = newTokenByCopying(token);
		tokenRead (token);
		if (token->type == '{')
		{
			vString *anonver = anonGenerateNew ("ver", K_VERSION);
			makeSimpleTag (anonver, K_VERSION);
			vStringDelete(anonver);
			tokenUnread (token);
			tokenSkipOverPair (curly);
			tokenCopy (token, curly);
			tokenDelete (curly);
			return;
		}
		tokenDelete (curly);
		tokenUnread (token);

		do {
			tokenRead (token);
			if (tokenIsType(token, IDENTIFIER))
			{
				parseVersion (token);
				tokenSkipToType (token, ';');
			}
		} while (! (tokenIsEOF (token) || token->type == '}'));
	}
}

static void findLdScriptTags (void)
{
	tokenInfo *const token = newLdScriptToken ();
	tokenInfo *const tmp = newLdScriptToken ();

	cppInit (false, false, false, false,
			 KIND_GHOST_INDEX, 0, 0,
			 KIND_GHOST_INDEX,
			 KIND_GHOST_INDEX, 0, 0,
			 FIELD_UNKNOWN);

	do {
		tokenRead (token);
		if (tokenIsKeyword (token, ENTRY))
			parseEntry (token);
		else if (tokenIsKeyword (token, SECTIONS))
			parseSections (token);
		else if (tokenIsType(token, IDENTIFIER))
		{
			tagEntryInfo e;
			tokenRead (tmp);
			if (tokenIsType(tmp, ASSIGNMENT_OP))
			{
				makeLdScriptTagMaybe (&e, token,
									  K_SYMBOL, ROLE_DEFINITION_INDEX);
				tokenSkipToType (tmp, ';');
			}
		}
		else if (tokenIsKeyword (token, VERSION))
			parseVersions(token);
	} while (!tokenIsEOF (token));

	cppTerminate ();

	tokenDelete (tmp);
	tokenDelete (token);

	flashTokenBacklog (&ldScriptTokenInfoClass);
}

static void initialize (const langType language)
{
	Lang_ldscript = language;
}

extern parserDefinition* LdScriptParser (void)
{
	parserDefinition* def = parserNew ("LdScript");

	/* File name patters are picked from Linux kernel and ecos. */
	static const char *const extensions [] = { "lds", "scr", "ld", "ldi", NULL };

	/* lds.S must be here because Asm parser registers .S as an extension.
	 * ld.script is used in linux/arch/mips/boot/compressed/ld.script. */
	static const char *const patterns [] = { "*.lds.S", "ld.script", NULL };

	/* Emacs's mode */
	static const char *const aliases [] = { "ld-script", NULL };

	def->initialize = initialize;
	def->parser     = findLdScriptTags;

	def->kindTable      = LdScriptKinds;
	def->kindCount  = ARRAY_SIZE (LdScriptKinds);
	def->extensions = extensions;
	def->patterns   = patterns;
	def->aliases    = aliases;
	def->keywordTable = LdScriptKeywordTable;
	def->keywordCount = ARRAY_SIZE (LdScriptKeywordTable);
	def->fieldTable = LdScriptFields;
	def->fieldCount = ARRAY_SIZE (LdScriptFields);

	def->useCork    = CORK_QUEUE|CORK_SYMTAB;

	def->versionCurrent = 1;
	def->versionAge = 1;

	return def;
}
