/*
 *   Copyright (c) 2011, Colomban Wendling <colomban@geany.org>
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 *   This module contains functions for generating tags for M4.
 */

#include "general.h"	/* must always come first */

#include <ctype.h>
#include <string.h>
#include <stdio.h>

#include "entry.h"
#include "htable.h"
#include "keyword.h"
#include "m4.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"


enum M4Kind {
	M4_MACRO_KIND,
	M4_MACROFILE_KIND,
};

enum M4MacroRole {
	M4_MACRO_ROLE_UNDEF,
};

enum M4MacrofileRole {
	M4_MACROFILE_ROLE_INCLUDED,
	M4_MACROFILE_ROLE_SILENTLY_INCLUDED,
};


static roleDefinition M4MacroRoles [] = {
	{ true, "undef", "undefined" },
};

static roleDefinition M4MacrofileRoles [] = {
	{ true, "included", "included macro" },
	{ true, "sincluded", "silently included macro" },
};

static kindDefinition M4Kinds[] = {
	{ true, 'd', "macro", "macros",
	  .referenceOnly = false, ATTACH_ROLES(M4MacroRoles) },
	{ true, 'I', "macrofile", "macro files",
	  .referenceOnly = true, ATTACH_ROLES(M4MacrofileRoles) },
};

typedef enum {
	KEYWORD_define,
	KEYWORD_undefine,
	KEYWORD_include,
	KEYWORD_sinclude,
	KEYWORD_changequote,
} m4KeywordId;

/* TODO: ideally "m4_" prefix keywords should be
   installed and handled in Autoconf parser. */
static const keywordTable m4KeywordTable[] = {
#define ENTRY(K) \
	{ #K, KEYWORD_##K }, \
	{ "m4_" #K, KEYWORD_##K }
	ENTRY(define),
	ENTRY(undefine),
	ENTRY(include),
	ENTRY(sinclude),
	ENTRY(changequote),
};


/* Quote handling */

/* TODO: Characters are assumed for quoting.
   However, m4 allows strings. */
static char m4QuoteOpen = 0;
static char m4QuoteClose = 0;

extern void setM4Quotes(char openQuote, char closeQuote)
{
	m4QuoteOpen = openQuote;
	m4QuoteClose = closeQuote;
}

/* gets the close quote corresponding to openQuote.
 * return 0 if openQuote is not a valid open quote */
static int getCloseQuote(int openQuote)
{
	if (openQuote == m4QuoteOpen)
	{
		return m4QuoteClose;
	}
	return 0;
}

static void skipQuotes(int c)
{
	unsigned int depth = 0;
	int openQuote = 0, closeQuote = 0;

	closeQuote = getCloseQuote(c);
	if (! closeQuote)
		return;
	else
		openQuote = c;

	for (; c != EOF; c = getcFromInputFile())
	{
		if (c == closeQuote)
			depth --;
		else if (c == openQuote)
			depth ++;
		if (depth == 0)
			break;
	}
}


/* parser */

#define IS_WORD(c) (isalnum(c) || (c) == '_')

/* reads a possibly quoted word.  word characters are those passing IS_WORD() */
static void readQuotedWord(vString *const name)
{
	unsigned int depth = 0;
	int openQuote = 0, closeQuote = 0;
	int c = getcFromInputFile();

	closeQuote = getCloseQuote(c);
	if (closeQuote != 0)
	{
		openQuote = c;
		depth ++;
		c = getcFromInputFile();
	}

	for (; c != EOF; c = getcFromInputFile())
	{
		/* don't allow embedded NULs, and prevents to match when quote == 0 (aka none) */
		if (c == 0)
			break;
		/* close before open to support open and close characters to be the same */
		else if (c == closeQuote)
			depth --;
		else if (c == openQuote)
			depth ++;
		else if (IS_WORD(c) || depth > 0)
			vStringPut(name, c);
		else
		{
			ungetcToInputFile(c);
			break;
		}
	}
}

static bool skipLineEnding(int c)
{
	if (c == '\n')
		return true;
	else if (c == '\r')
	{
		/* try to eat the `\n' of a `\r\n' sequence */
		c = getcFromInputFile();
		if (c != '\n')
			ungetcToInputFile(c);
		return true;
	}

	return false;
}

static void skipToCharacter(int ch, bool oneLine)
{
	int c;

	while ((c = getcFromInputFile()) != EOF)
	{
		if (c == ch)
			break;
		else if (oneLine && skipLineEnding(c))
			break;
	}
}

static void skipLine(int c)
{
	for (; c != EOF; c = getcFromInputFile())
	{
		if (skipLineEnding(c))
			break;
	}
}

static m4Subparser * maySwitchLanguage (const char* token)
{
	subparser *tmp;
	m4Subparser *m4found = NULL;

	foreachSubparser (tmp, false)
	{
		m4Subparser *m4tmp = (m4Subparser *)tmp;

		enterSubparser(tmp);
		if (m4tmp->probeLanguage
			&& m4tmp->probeLanguage (m4tmp, token))
		{
			chooseExclusiveSubparser (tmp, NULL);
			m4found = m4tmp;
		}
		leaveSubparser();

		if (m4found)
			break;
	}

	return m4found;
}

/* reads everything in a macro argument
 * return true if there are more args, false otherwise */
extern bool readM4MacroArgument(vString *const arg)
{
	int c;

	/* discard leading blanks */
	while ((c = getcFromInputFile()) != EOF && isspace(c))
		;

	for (; c != EOF; c = getcFromInputFile())
	{
		if (c == ',' || c == ')')
		{
			ungetcToInputFile(c);
			return c == ',';
		}
		else if (getCloseQuote(c) != 0)
		{
			ungetcToInputFile(c);
			readQuotedWord(arg);
		}
		else
			vStringPut(arg, c);
	}

	return false;
}

static void handleM4Changequote(void)
{
	vString *const arg = vStringNew();
	char args[2] = {0,0};
	int i, n = (sizeof(args) / sizeof(args[0]));
	bool more = true;

	for (i = 0; more && i < n; i++)
	{
		const char *v;

		vStringClear(arg);
		more = readM4MacroArgument(arg);
		if (more)
			getcFromInputFile();
		v = vStringValue(arg);
		if (! v[0] || v[1])
			break;
		else
			args[i] = *v;
	}

	if (! more)
	{
		if (args[0] && args[1])
			setM4Quotes (args[0], args[1]);
		else if (args[1])
			setM4Quotes (args[0], '\'');
		else if (args[0])
			setM4Quotes ('\0', '\0');
		else
			setM4Quotes ('`', '\'');
	}

	vStringDelete(arg);
}

static bool doesQuoteStart (int c)
{
	return (c == m4QuoteOpen);
}

static bool doesLineCommentStart (m4Subparser *m4, int c, char *token)
{
	if (m4 && m4->doesLineCommentStart)
	{
		bool r;
		enterSubparser ((subparser *)m4);
		r = m4->doesLineCommentStart (m4, c, token);
		leaveSubparser ();
		if (r)
			return true;
	}

	return (strcmp(token, "dnl") == 0);
}

static bool doesStringLiteralStart (m4Subparser *m4, int c)
{
	if (m4 && m4->doesStringLiteralStart)
	{
		bool r;
		enterSubparser ((subparser *)m4);
		r = m4->doesStringLiteralStart (m4, c);
		leaveSubparser ();
		return r;
	}
	return false;
}

static int notifyNewMacro (m4Subparser *m4, const char *token)
{
	int index;

	enterSubparser ((subparser *)m4);
	index = m4->newMacroNotify (m4, token);
	leaveSubparser ();

	return index;
}

/* tag creation */

static int makeM4RefTag(int kind, const vString *const name, int role)
{
	tagEntryInfo e;

	if (vStringLength(name) <= 0)
		return CORK_NIL;

	initRefTagEntry (&e, vStringValue(name), kind, role);

	return makeTagEntry(&e);
}

static int makeM4Tag (int kind, int role)
{
	int index = CORK_NIL;
	vString *name = NULL;

	if (kind == M4_MACRO_KIND)
	{
		if (role == ROLE_DEFINITION_INDEX)
		{
			name = vStringNew();
			readM4MacroArgument(name);
			index = makeM4RefTag (kind, name, role);
		}
		else if (role == M4_MACRO_ROLE_UNDEF)
		{
			name = vStringNew();
			while (true)
			{
				bool more = readM4MacroArgument(name);
				/* TODO: The cork indexes are thrown away here.
				   `end' field cannot be attached to multiple
				   indexes. */
				makeM4RefTag (kind, name, role);
				vStringClear (name);
				if (more)
					getcFromInputFile ();
				else
					break;
			}

		}
	}
	else if (kind == M4_MACROFILE_KIND)
	{
		name = vStringNew();
		readM4MacroArgument(name);
		index = makeM4RefTag (kind, name, role);
	}

	if (name)
		vStringDelete (name);

	return index;
}

struct newMacroResult
{
	int index;
	bool consumed;
};

static struct newMacroResult newMacroM4 (const char* token)
{
	static langType lang = LANG_IGNORE;
	struct newMacroResult result = {
		.index = CORK_NIL,
		.consumed = false,
	};

	int keyword;
	int role = ROLE_DEFINITION_INDEX;
	int kind = -1;

	if (lang == LANG_IGNORE)
		lang = getNamedLanguage ("M4", 0);
	keyword = lookupKeyword (token, lang);

	switch (keyword)
	{
	case KEYWORD_NONE:
		break;
	case KEYWORD_define:
		kind = M4_MACRO_KIND;
		role = ROLE_DEFINITION_INDEX;
		result.consumed = true;
		break;
	case KEYWORD_undefine:
		kind = M4_MACRO_KIND;
		role = M4_MACRO_ROLE_UNDEF;
		result.consumed = true;
		break;
	case KEYWORD_include:
		kind = M4_MACROFILE_KIND;
		role = M4_MACROFILE_ROLE_INCLUDED;
		result.consumed = true;
		break;
	case KEYWORD_sinclude:
		kind = M4_MACROFILE_KIND;
		role = M4_MACROFILE_ROLE_SILENTLY_INCLUDED;
		result.consumed = true;
		break;
	case KEYWORD_changequote:
		handleM4Changequote ();
		result.consumed = true;
		break;
	}

	if (kind == -1)
		return result;

	if ((! isXtagEnabled (XTAG_REFERENCE_TAGS))
	    && (role != ROLE_DEFINITION_INDEX))
		return result;

	result.index = makeM4Tag (kind, role);
	return result;
}


/* parser instance  */

static void findM4Tags(void)
{
	m4Subparser *sub;
	vString *const token = vStringNew();
	int c;
	int index = CORK_NIL;

	setM4Quotes ('`', '\'');

	sub = (m4Subparser *)getSubparserRunningBaseparser();
	if (sub)
		chooseExclusiveSubparser ((subparser *)sub, NULL);

	while ((c = getcFromInputFile()) != EOF)
	{
		if (doesLineCommentStart (sub, c, vStringValue (token)))
			skipLine(c);
		else if (doesQuoteStart (c))
			skipQuotes(c);
		else if (doesStringLiteralStart (sub, c))
			skipToCharacter(c, false);
		else if (c == '(' && vStringLength(token) > 0) /* catch a few macro calls */
		{
			struct newMacroResult r;

			if (!sub)
				sub = maySwitchLanguage (vStringValue (token));

			r = newMacroM4 (vStringValue (token));
			if (r.consumed)
				index = r.index;
			else if (sub)
				index = notifyNewMacro (sub, vStringValue (token));
		}

		vStringClear(token);
		if (IS_WORD(c))
		{
			ungetcToInputFile(c);
			readQuotedWord(token);
		}
		else if (c == ')')
		{
			tagEntryInfo *e = getEntryInCorkQueue (index);
			if (e)
				e->extensionFields.endLine = getInputLineNumber ();
			index = CORK_NIL;
		}
	}

	vStringDelete(token);
}

extern parserDefinition* M4Parser (void)
{
	static const char *const extensions [] = { "m4",
						   "spt", /* used in `selinux-policy' */
						   NULL };
	parserDefinition* const def = parserNew("M4");

	def->kindTable = M4Kinds;
	def->kindCount = ARRAY_SIZE(M4Kinds);
	def->extensions = extensions;
	def->parser = findM4Tags;
	def->useCork = CORK_QUEUE;
	def->keywordTable = m4KeywordTable;
	def->keywordCount = ARRAY_SIZE (m4KeywordTable);

	return def;
}
