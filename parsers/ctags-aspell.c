/*
 *   ctags-aspell.c
 *
 *   Copyright (c) 2017, Masatake YAMATO <yamato@redhat.com>
 *   Copyright (c) 2017, Red Hat, K.K.
 *
 *   This source code is released for free distribution under the terms of the
 *   GNU General Public License version 2 or (at your option) any later version.
 *
 */

/* This is not a normal parser. This is specialized parser for maintaining
   Universal-Ctags itself. */

/*
  This parser reports a spell mistake as a reference tag:
  "word" kind, "misspelled" role. */

#include "general.h"

#include "options.h"
#include "htable.h"
#include "keyword.h"
#include "kind.h"
#include "mio.h"
#include "parse.h"
#include "read.h"
#include "strlist.h"
#include "tokeninfo.h"

#include <string.h>

#ifdef HAVE_ASPELL
#include <aspell.h>
#else
typedef void AspellSpeller;
#endif

#define DEBUG_WORD false

typedef enum {
	K_WORD,
} AspellParserKind;

typedef enum {
	R_WORD_MISSPELLED,
	R_WORD_TRACED
} AspellWordRole;

static roleDefinition AspellWordRoles [] = {
	{ true,       "misspelled", "misspelled" },
	{ DEBUG_WORD, "traced", "traced by this parser (enabling/disabling in build-time)" },
};

static kindDefinition AspellParserKinds [] = {
	{true, 'w', "word", "words",
	 .referenceOnly = false, ATTACH_ROLES(AspellWordRoles)},
};


static void aspell_dictfile_handler (const langType language CTAGS_ATTR_UNUSED,
									 const char *optname CTAGS_ATTR_UNUSED,
									 const char *arg);
static void aspell_dictword_handler (const langType language CTAGS_ATTR_UNUSED,
									 const char *optname CTAGS_ATTR_UNUSED,
									 const char *arg);
static void aspell_subwords_handler (const langType language CTAGS_ATTR_UNUSED,
									 const char *optname,
									 const char *arg);

static parameterHandlerTable AspellParameterHandlerTable [] = {
	{ .name = "subwords",
	  .desc = "split a word into subwords before spell checking: true or false",
	  .handleParameter = aspell_subwords_handler,
	},
	{ .name = "dictfile",
	  .desc = "line oriented words list; FILENAME is expected, accumulative",
	  .handleParameter = aspell_dictfile_handler,
	},
	{ .name = "dictword",
	  .desc = "known correctly spelled word",
	  .handleParameter = aspell_dictword_handler,
	},
};

enum eTokenType {
	/* 0..255 are the byte's value */
	TOKEN_EOF = 256,
	TOKEN_UNDEFINED,
	TOKEN_WORD,
	TOKEN_KEYWORD,
};

static void readAspellToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED);

static struct tokenInfoClass aspellTokenInfoClass = {
	.nPreAlloc = 2,
	.typeForUndefined = TOKEN_UNDEFINED,
	.keywordNone      = KEYWORD_NONE,
	.typeForKeyword   = TOKEN_KEYWORD,
	.typeForEOF       = TOKEN_EOF,
	.extraSpace       = 0,
	.read             = readAspellToken,
};

static tokenInfo *newAspellToken (void)
{
	return newToken (&aspellTokenInfoClass);
}

static void readAspellToken (tokenInfo *const token, void *data CTAGS_ATTR_UNUSED)
{
	int c;

	token->type		= TOKEN_UNDEFINED;
	token->keyword	= KEYWORD_NONE;
	vStringClear (token->string);

	while (1)
	{
		c = getcFromInputFile();
		if (c == EOF)
		{
			token->type = TOKEN_EOF;
			return;
		}
		else if (isalpha (c))
			break;
	}

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	token->type = TOKEN_WORD;
	tokenPutc(token, c);

	while ((c = getcFromInputFile ()) != EOF)
	{
		if (isalnum(c))
			tokenPutc(token, c);
		else
			break;
	}
}

static void downcase_word (char *word)
{
	for (unsigned int i = 0; word[i] != '\0'; i++)
	{
		if (isupper (word[i]))
			word[i] = (word[i] - ('A' - 'a'));
	}
}

static stringList *word2subwords (const char* word, bool split)
{
	if (split)
		return stringListNewBySplittingWordIntoSubwords (word);
	else
	{
		stringList *list = stringListNew ();
		stringListAdd (list, vStringNewInit(word));
		return list;
	}
}

static bool splittingIntoSubwords;
static void aspell_subwords_handler (const langType language CTAGS_ATTR_UNUSED,
									 const char *name,
									 const char *arg)
{
	splittingIntoSubwords = paramParserBool (arg, splittingIntoSubwords,
											 name, "parameter");
}

static int add_word_to_dict (const char *word, hashTable * dict, bool split)
{
	int count = 0;

	stringList * list = word2subwords (word, splittingIntoSubwords);
	for (unsigned int j = 0; j < stringListCount (list); j++)
	{
		vString *item = stringListItem (list, j);
		if (!hashTableHasItem (dict, vStringValue(item)))
		{
			char* subword = eStrdup (vStringValue(item));
			downcase_word (subword);
			hashTablePutItem (dict, subword, subword);
			count++;
		}
	}
	stringListDelete(list);
	return count;
}

static hashTable *userDict;

static hashTable *makeDict (void)
{
	return hashTableNew (257, hashCstrhash, hashCstreq, eFree, NULL);
}

static void aspell_dictfile_handler (const langType language CTAGS_ATTR_UNUSED,
									 const char *optname CTAGS_ATTR_UNUSED,
									 const char *arg)
{
	int count = 0;

	if (Option.interactive == INTERACTIVE_SANDBOX)
	{
		verbose ("UCtagsAspell: loading a user dictionary is disallowed in sandbox: %s\n", arg);
		return;
	}

	MIO *mio = mio_new_file (arg, "r");

	if (!mio)
	{
		error(WARNING, "Failed to open file \"%s\" as a dictionary", arg);
		return;
	}

	verbose ("UCtagsAspell: loading a user dictionary: %s\n", arg);

	if (!userDict)
		userDict = makeDict();

	vString *line = vStringNew();

	while (readLineRaw (line, mio))
	{
		char *word = vStringValue (line);

		if (word[0] == '\0' || word[0] == '\n' || word[0] == '#')
			continue;

		for (unsigned int i = vStringLength(line); 0 < i; i--)
			if (isalnum (word[i - 1]))
				break;
			else
				word [i - 1] = '\0';

		count += add_word_to_dict(word, userDict, splittingIntoSubwords);

	}

	verbose ("UCtagsAspell: load %d word(s)\n", count);

	vStringDelete (line);

	mio_free (mio);
}

static void aspell_dictword_handler (const langType language CTAGS_ATTR_UNUSED,
									 const char *optname CTAGS_ATTR_UNUSED,
									 const char *arg)
{
	int count = 0;

	if (!userDict)
		userDict = makeDict();

	if (arg && arg[0] != '\0')
		count += add_word_to_dict(arg, userDict, splittingIntoSubwords);

	verbose ("UCtagsAspell: add %d word(s)\n", count);
}

#ifdef HAVE_ASPELL
static AspellConfig * spell_config;
#endif

static void initialize (const langType language CTAGS_ATTR_UNUSED)
{
	if (Option.interactive == INTERACTIVE_SANDBOX)
	{
		verbose ("UCtagsAspell: initializing aspell is disallowed in sandbox\n");
		return;
	}

#ifdef HAVE_ASPELL
	spell_config = new_aspell_config();
	aspell_config_replace(spell_config, "lang", "en_US");
#endif
}

static void finalize (langType language CTAGS_ATTR_UNUSED, bool initialized)
{
	if (initialized)
	{
#ifdef HAVE_ASPELL
		if (spell_config)
			delete_aspell_config (spell_config);
#endif

		hashTableDelete (userDict);
		userDict = NULL;
	}
}

static bool isPartOfUUID (const char *word)
{
	const char *cursor;

	for (cursor = word; *cursor != '\0'; cursor++)
	{
		/* TODO: reject [A-F]. */
		if (isxdigit(*cursor))
			continue;

		return false;
	}
	return true;
}

static bool check_spell (AspellSpeller * spell_checker,
						 hashTable *htable,
						 const char* word)
{
	if (isPartOfUUID (word))
		return true;

	if (htable && hashTableGetItem (htable, word))
		return true;

#ifdef HAVE_ASPELL
		if (aspell_speller_check (spell_checker,
								  word, -1))
			return true;
#endif

		return false;
}

static stringList *token2words (const tokenInfo * token, bool split)
{
	return word2subwords(tokenString (token), split);
}

static void findMisspelling (void)
{
	AspellSpeller * spell_checker = NULL;

#ifdef HAVE_ASPELL
	if (Option.interactive != INTERACTIVE_SANDBOX)
	{
		AspellCanHaveError * possible_err = new_aspell_speller(spell_config);
		if (aspell_error_number(possible_err) != 0)
		{
			error(WARNING, "Failed to create aspell speller object(new_aspell_speller): %s",
				  aspell_error_message(possible_err));
			return;
		}
		spell_checker = to_aspell_speller(possible_err);
	}
#endif

	tokenInfo *const token = newAspellToken ();

	do {
		tokenRead (token);
		stringList *words = token2words (token, splittingIntoSubwords);

		for (unsigned int i = 0; i < stringListCount (words); i++)
		{
			vString* item = stringListItem (words, i);
			char *word = vStringValue (item);

			makeSimpleRefTag (item,
							  K_WORD,
							  R_WORD_TRACED);

			downcase_word (word);

			if (check_spell (spell_checker, userDict, word))
				continue;

			makeSimpleRefTag (item,
							  K_WORD,
							  R_WORD_MISSPELLED);
		}
		stringListDelete (words);

	} while (!tokenIsEOF(token));

	tokenDestroy (token);
	flashTokenBacklog (&aspellTokenInfoClass);

#ifdef HAVE_ASPELL
	if (spell_checker)
		delete_aspell_speller(spell_checker);
#endif
}

extern parserDefinition* UCtagsAspellParser (void)
{
	parserDefinition* const def = parserNew ("UCtagsAspell");

	def->invisible  = true;
	def->enabled    = false;
	def->kindTable  = AspellParserKinds;
	def->kindCount  = ARRAY_SIZE (AspellParserKinds);
	def->initialize = initialize;
	def->finalize   = finalize;
	def->parser     = findMisspelling;
	def->parameterHandlerTable = AspellParameterHandlerTable;
	def->parameterHandlerCount = ARRAY_SIZE(AspellParameterHandlerTable);

	return def;
}
