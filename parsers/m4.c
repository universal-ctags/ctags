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
#include "options.h"
#include "parse.h"
#include "read.h"
#include "vstring.h"

/*
 * M4 meta parser
 */

static hashTable *m4ParserClients;

/* Quote handling */

/* TODO: Characters are assumed for quoting.
   However, m4 allows strings. */
static char m4QuoteOpen = 0;
static char m4QuoteClose = 0;

static void setM4Quotes(char openQuote, char closeQuote)
{
	m4QuoteOpen = openQuote;
	m4QuoteClose = closeQuote;
}

/* gets the close quote corresponding to openQuote.
 * return 0 if openQuote is not a valid open quote */
static int getCloseQuote(int openQuote)
{
	if (openQuote == m4QuoteOpen)
		return m4QuoteClose;
	return 0;

	switch (openQuote)
	{
		case '[': return ']';
		case '`': return '\'';
		case '\'':
		case '"': return openQuote;

		default: return 0;
	}
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

static boolean skipLineEnding(int c)
{
	if (c == '\n')
		return TRUE;
	else if (c == '\r')
	{
		/* try to eat the `\n' of a `\r\n' sequence */
		c = getcFromInputFile();
		if (c != '\n')
			ungetcToInputFile(c);
		return TRUE;
	}

	return FALSE;
}

static void skipToCharacter(int ch, boolean oneLine)
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

static void prepareForNewInput (struct m4ParserClient *client)
{
	do {
		if (client->prepareForNewInput)
			client->data = client->prepareForNewInput ();
		client = client->host;
	} while (client);
}

struct probeData {
	struct m4ParserClient *client;
	boolean found;
	const char* token;
};

static void mayRunProbe (void *key CTAGS_ATTR_UNUSED, void *value, void* user_data)
{
	struct probeData *probe_data = user_data;
	struct m4ParserClient *client = value;

	if (probe_data->found)
		return;

	if (client->host != probe_data->client)
		return;

	if (client->probeLanguage && client->probeLanguage (probe_data->token))
	{
		probe_data->client = client;
		probe_data->found = TRUE;
	}
}

static struct m4ParserClient * maySwitchLanguage (struct m4ParserClient *client,
						  vString *token)
{
	struct probeData probe_data = {
		.client = client,
		.found  = FALSE,
		.token  = vStringValue (token),
	};

	hashTableForeachItem (m4ParserClients, mayRunProbe, &probe_data);

	if (probe_data.client != client)
	{
		prepareForNewInput (probe_data.client);
		setM4Quotes (probe_data.client->quoteOpen,
			     probe_data.client->quoteClose);
		/* This is an unbalanced push.
		   This is for specifying the language name
		   in the file tag for the current input.
		   (The file tag is enabled with --extra=+f.) */
		if (isLanguageEnabled (probe_data.client->lang))
			pushLanguage (probe_data.client->lang);
	}

	return probe_data.client;
}

/* reads everything in a macro argument
 * return TRUE if there are more args, FALSE otherwise */
extern boolean readM4MacroArgument(vString *const arg)
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

	return FALSE;
}

static void handleM4Changequote(void)
{
	vString *const arg = vStringNew();
	char args[2] = {0,0};
	int i, n = (sizeof(args) / sizeof(args[0]));
	boolean more = TRUE;

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

extern void registerM4ParserClient (const char *hostLang, struct m4ParserClient *client)
{
	langType host = LANG_IGNORE;

	if (!m4ParserClients)
		m4ParserClients = hashTableNew (5, hashInthash, hashInteq,
						NULL, NULL);

	if (hostLang)
	{
		host = getNamedLanguage (hostLang, 0);
		if (host == LANG_IGNORE)
			error(FATAL, "M4: No such language: %s\n", hostLang);
	}

	if (host != LANG_IGNORE)
		initializeParser (host);

	client->host = hashTableGetItem (m4ParserClients, &host);

	verbose ("Register %s parser as M4 subparser\n", getLanguageName (client->lang));
	hashTablePutItem (m4ParserClients, &client->lang, client);
}

static boolean doesStartLineComment (struct m4ParserClient *client, int c, const char* token)
{
	struct m4ParserClient *current;
	for (current = client; current; current = current->host)
	{
		if (current->doesStartLineComment (c, token, client->data))
			return TRUE;
	}
	return FALSE;
}

static boolean doesStartQuote (int c)
{
	return (c == m4QuoteOpen);
}

static boolean doesStartStringLiteral (struct m4ParserClient *client, int c)
{
	struct m4ParserClient *current;
	for (current = client; current; current = current->host)
	{
		if (current->doesStartStringLiteral
		    && current->doesStartStringLiteral (c, client->data))
		    return TRUE;
	}
	return FALSE;
}

static int handleMacro (struct m4ParserClient *client, const char *token)
{
	struct m4ParserClient *current;
	struct m4HandleTokenResult result = {
		.index = CORK_NIL,
		.consumed = FALSE,
	};

	for (current = client; current; current = current->host)
	{
		pushLanguage (current->lang);
		result = current->handleMacro (token, current->data);
		popLanguage ();

		if (result.index != CORK_NIL)
			break;
		else if (result.consumed)
			break;
	}
	return result.index;
}

extern void runM4Parser (langType lang)
{
	struct m4ParserClient *client;
	vString *const token = vStringNew();
	int c;
	int index = CORK_NIL;

	client = hashTableGetItem (m4ParserClients, &lang);
	if (client == NULL)
		return;

	prepareForNewInput (client);
	setM4Quotes (client->quoteOpen, client->quoteClose);

	while ((c = getcFromInputFile()) != EOF)
	{
		if (doesStartLineComment (client, c, vStringValue (token)))
			skipLine(c);
		else if (doesStartQuote (c))
			skipQuotes(c);
		else if (doesStartStringLiteral (client, c))
			skipToCharacter(c, FALSE);
		else if (c == '(' && vStringLength(token) > 0) /* catch a few macro calls */
		{
			client = maySwitchLanguage (client, token);
			index = handleMacro (client, vStringValue (token));
		}

		vStringClear(token);
		if (IS_WORD(c))
		{
			ungetcToInputFile(c);
			readQuotedWord(token);
		}
		else if (c == ')'&& (index != CORK_NIL))
		{
			tagEntryInfo *e = getEntryInCorkQueue (index);
			e->extensionFields.endLine = getInputLineNumber ();
			index = CORK_NIL;
		}
	}
	vStringDelete(token);
}

/*
 * M4 concrete parser
 */

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


static roleDesc M4MacroRoles [] = {
	{ TRUE, "undef", "undefined" },
};

static roleDesc M4MacrofileRoles [] = {
	{ TRUE, "included", "included macro" },
	{ TRUE, "sincluded", "silently included macro" },
};

static kindOption M4Kinds[] = {
	{ TRUE, 'd', "macro", "macros",
	  .referenceOnly = FALSE, ATTACH_ROLES(M4MacroRoles) },
	{ TRUE, 'I', "macrofile", "macro files",
	  .referenceOnly = TRUE, ATTACH_ROLES(M4MacrofileRoles) },
};

typedef enum {
	KEYWORD_define,
	KEYWORD_undefine,
	KEYWORD_include,
	KEYWORD_sinclude,
	KEYWORD_changequote,
} m4KeywordId;

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

/* tag creation */

static int makeM4RefTag(const kindOption *kind, const vString *const name, int role)
{
	tagEntryInfo e;
	static langType lang = LANG_IGNORE;

	if (vStringLength(name) <= 0)
		return CORK_NIL;

	if (lang == LANG_IGNORE)
		lang = getNamedLanguage ("M4", 0);

	if (! isLanguageEnabled (lang))
		return CORK_NIL;

	initRefTagEntry (&e, vStringValue(name), kind, role);

	return makeTagEntry(&e);
}

static int makeM4Tag(const kindOption *kind, const vString *const name)
{
	return makeM4RefTag (kind, name, ROLE_INDEX_DEFINITION);
}

/* methods for adapting to meta parser  */

static void *m4PrepareForNewInput (void)
{
	static int role;

	role = ROLE_INDEX_DEFINITION;
	return &role;
}

static boolean m4DoesStartLineComment (int c, const char* token, void *data)
{
	return (strcmp(token, "dnl") == 0);
}

static int m4MakeTag (int kind, int role)
{
	int index = CORK_NIL;
	vString *name = NULL;

	if (kind == M4_MACRO_KIND)
	{
		if (role == ROLE_INDEX_DEFINITION)
		{
			name = vStringNew();
			readM4MacroArgument(name);
			index = makeM4Tag (M4Kinds + kind, name);
		}
		else if (role == M4_MACRO_ROLE_UNDEF)
		{
			name = vStringNew();
			while (true)
			{
				boolean more = readM4MacroArgument(name);
				/* TODO: The cork indexes are thrown away here.
				   `end' field cannot be attached to multiple
				   indexes. */
				makeM4RefTag (M4Kinds + kind, name, role);
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
		index = makeM4RefTag (M4Kinds + kind, name, role);
	}

	if (name)
		vStringDelete (name);

	return index;
}

static struct m4HandleTokenResult m4HandleMacro (const char* token, void *data)
{
	static langType lang = LANG_IGNORE;
	struct m4HandleTokenResult result = {
		.index = CORK_NIL,
		.consumed = FALSE,
	};

	int keyword;
	int role = ROLE_INDEX_DEFINITION;
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
		role = ROLE_INDEX_DEFINITION;
		result.consumed = TRUE;
		break;
	case KEYWORD_undefine:
		kind = M4_MACRO_KIND;
		role = M4_MACRO_ROLE_UNDEF;
		result.consumed = TRUE;
		break;
	case KEYWORD_include:
		kind = M4_MACROFILE_KIND;
		role = M4_MACROFILE_ROLE_INCLUDED;
		result.consumed = TRUE;
		break;
	case KEYWORD_sinclude:
		kind = M4_MACROFILE_KIND;
		role = M4_MACROFILE_ROLE_SILENTLY_INCLUDED;
		result.consumed = TRUE;
		break;
	case KEYWORD_changequote:
		handleM4Changequote ();
		result.consumed = TRUE;
		break;
	}

	if (kind == -1)
		return result;

	if ((! isXtagEnabled (XTAG_REFERENCE_TAGS))
	    && (role != ROLE_INDEX_DEFINITION))
		return result;

	result.index = m4MakeTag (kind, role);
	return result;
}

static struct m4ParserClient M4M4Client = {
	/* the value will be updated when the M4 parser is initialized. */
	.lang = LANG_IGNORE,

	.quoteOpen = '`',
	.quoteClose = '\'',
	.prepareForNewInput = m4PrepareForNewInput,
	.doesStartLineComment = m4DoesStartLineComment,
	.doesStartStringLiteral = NULL,
	.probeLanguage = NULL,
	.handleMacro = m4HandleMacro,
};

/* parser instance  */

static void initializeM4Parser (const langType language)
{
	M4M4Client.lang = language;
	registerM4ParserClient (NULL, &M4M4Client);
}

static void findM4Tags(void)
{
	langType lang = getInputLanguage ();

	/* The other per-input initialization is done
	   in m4PrepareForNewInput. */
	runM4Parser (lang);
}

extern parserDefinition* M4Parser (void)
{
	static const char *const extensions [] = { "m4",
						   "spt", /* used in `selinux-policy' */
						   NULL };
	parserDefinition* const def = parserNew("M4");

	def->kinds = M4Kinds;
	def->kindCount = ARRAY_SIZE(M4Kinds);
	def->extensions = extensions;
	def->parser = findM4Tags;
	def->initialize = initializeM4Parser;
	def->useCork = 1;
	def->keywordTable = m4KeywordTable;
	def->keywordCount = ARRAY_SIZE (m4KeywordTable);

	return def;
}
