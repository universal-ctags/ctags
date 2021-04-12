/*
 *	 Copyright (c) 2008, David Fishburn
 *	 Copyright (c) 2012, Jan Larres
 *	 Copyright (c) 2019, Mirco Schönfeld
 *
 *	 This source code is released for free distribution under the terms of the
 *	 GNU General Public License version 2 or (at your option) any later version.
 *
 *	 This module contains functions for generating identifiers of entries of Bibtex language files.
 *
 *	 BibTex language "reference":
 *		 https://en.wikipedia.org/wiki/BibTeX
 */

/*
 *	 INCLUDE FILES
 */
#include "general.h"	/* must always come first */
#include <ctype.h>	/* to define isalpha () */
#include <string.h>

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
 *	 MACROS
 */
#define isType(token,t)		(bool) ((token)->type == (t))
#define isKeyword(token,k)	(bool) ((token)->keyword == (k))
#define isIdentChar(c) \
	(isalpha (c) || isdigit (c) || (c) == '_' || (c) == '-' || (c) == '+' || (c) == ':')

/*
 *	 DATA DECLARATIONS
 */

/*
 * Used to specify type of keyword.
 */
enum eKeywordId {
	KEYWORD_article,
	KEYWORD_book,
	KEYWORD_booklet,
	KEYWORD_conference,
	KEYWORD_inbook,
	KEYWORD_incollection,
	KEYWORD_inproceedings,
	KEYWORD_manual,
	KEYWORD_mastersthesis,
	KEYWORD_misc,
	KEYWORD_phdthesis,
	KEYWORD_proceedings,
	KEYWORD_string,
	KEYWORD_techreport,
	KEYWORD_unpublished
};
typedef int keywordId; /* to allow KEYWORD_NONE */

enum eTokenType {
	/* 0..255 are the byte's value.  Some are named for convenience */
	TOKEN_OPEN_CURLY = '{',
	/* above is special types */
	TOKEN_UNDEFINED = 256,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER
};
typedef int tokenType;

typedef struct sTokenInfo {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	unsigned long 	lineNumber;
	MIOPos 			filePosition;
} tokenInfo;

/*
 *	DATA DEFINITIONS
 */

static langType Lang_bib;

typedef enum {
	BIBTAG_ARTICLE,
	BIBTAG_BOOK,
	BIBTAG_BOOKLET,
	BIBTAG_CONFERENCE,
	BIBTAG_INBOOK,
	BIBTAG_INCOLLECTION,
	BIBTAG_INPROCEEDINGS,
	BIBTAG_MANUAL,
	BIBTAG_MASTERSTHESIS,
	BIBTAG_MISC,
	BIBTAG_PHDTHESIS,
	BIBTAG_PROCEEDINGS,
	BIBTAG_STRING,
	BIBTAG_TECHREPORT,
	BIBTAG_UNPUBLISHED,
	BIBTAG_COUNT
} bibKind;

static kindDefinition BibKinds [] = {
	{ true,  'a', "article",				"article"				},
	{ true,  'b', "book",						"book"					},
	{ true,  'B', "booklet",				"booklet"				},
	{ true,  'c', "conference",			"conference"		},
	{ true,  'i', "inbook",					"inbook"				},
	{ true,  'I', "incollection",		"incollection"	},
	{ true,  'j', "inproceedings",	"inproceedings"	},
	{ true,  'm', "manual",					"manual"				},
	{ true,  'M', "mastersthesis",	"mastersthesis"	},
	{ true,  'n', "misc",						"misc"					},
	{ true,  'p', "phdthesis",			"phdthesis"			},
	{ true,  'P', "proceedings",		"proceedings"		},
	{ true,  's', "string",					"string"				},
	{ true,  't', "techreport",			"techreport"		},
	{ true,  'u', "unpublished",		"unpublished"		}
};

static const keywordTable BibKeywordTable [] = {
	/* keyword			  keyword ID */
	{ "article",	    KEYWORD_article				},
	{ "book",	        KEYWORD_book				  },
	{ "booklet",	    KEYWORD_booklet				},
	{ "conference",	  KEYWORD_conference		},
	{ "inbook",	      KEYWORD_inbook				},
	{ "incollection",	KEYWORD_incollection	},
	{ "inproceedings",KEYWORD_inproceedings	},
	{ "manual",	      KEYWORD_manual				},
	{ "mastersthesis",KEYWORD_mastersthesis	},
	{ "misc",	        KEYWORD_misc				  },
	{ "phdthesis",	  KEYWORD_phdthesis			},
	{ "proceedings",	KEYWORD_proceedings		},
	{ "string",				KEYWORD_string				},
	{ "techreport",	  KEYWORD_techreport		},
	{ "unpublished",	KEYWORD_unpublished		}
};

/*
 *	 FUNCTION DEFINITIONS
 */

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->string		= vStringNew ();
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	eFree (token);
}

/*
 *	 Tag generation functions
 */
static void makeBibTag (tokenInfo *const token, bibKind kind)
{
	if (BibKinds [kind].enabled)
	{
		const char *const name = vStringValue (token->string);
		tagEntryInfo e;
		initTagEntry (&e, name, kind);

		e.lineNumber   = token->lineNumber;
		e.filePosition = token->filePosition;

		makeTagEntry (&e);
	}
}

/*
 *	 Parsing functions
 */

/*
 *	Read a C identifier beginning with "firstChar" and places it into
 *	"name".
 */
static void parseIdentifier (vString *const string, const int firstChar)
{
	int c = firstChar;
	Assert (isIdentChar (c));
	do
	{
		vStringPut (string, c);
		c = getcFromInputFile ();
	} while (c != EOF && isIdentChar (c));
	if (c != EOF)
		ungetcToInputFile (c);		/* unget non-identifier character */
}

static bool readToken (tokenInfo *const token)
{
	int c;

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:

	do
	{
		c = getcFromInputFile ();
	}
	while (c == '\t' || c == ' ' || c == '\n');

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	token->type = (unsigned char) c;
	switch (c)
	{
		case EOF: return false;

		case '@':
					/*
					 * All Bib entries start with an at symbol.
					 * Check if the next character is an alpha character
					 * else it is not a potential tex tag.
					 */
					c = getcFromInputFile ();
					if (! isalpha (c))
					  ungetcToInputFile (c);
					else
					{
						vStringPut (token->string, '@');
						parseIdentifier (token->string, c);
						token->keyword = lookupCaseKeyword (vStringValue (token->string) + 1, Lang_bib);
						if (isKeyword (token, KEYWORD_NONE))
							token->type = TOKEN_IDENTIFIER;
						else
							token->type = TOKEN_KEYWORD;
					}
					break;
		case '%':
					skipToCharacterInInputFile ('\n'); /* % are single line comments */
					goto getNextChar;
					break;
		default:
					if (isIdentChar (c))
					{
						parseIdentifier (token->string, c);
						token->type = TOKEN_IDENTIFIER;
					}
					break;
	}
	return true;
}

static void copyToken (tokenInfo *const dest, tokenInfo *const src)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	vStringCopy (dest->string, src->string);
}

/*
 *	 Scanning functions
 */

static bool parseTag (tokenInfo *const token, bibKind kind)
{
	tokenInfo *	const name = newToken ();
	vString *		currentid;
	bool				eof = false;

	currentid = vStringNew ();
	/*
	 * Bib entries are of these formats:
	 *   @article{identifier,
	 *   author="John Doe"}
	 *
	 * When a keyword is found, loop through all words up to
	 * a comma brace for the tag name.
	 *
	 */
	if (isType (token, TOKEN_KEYWORD))
	{
		copyToken (name, token);
		if (!readToken (token))
		{
			eof = true;
			goto out;
		}
	}

	if (isType (token, TOKEN_OPEN_CURLY))
	{
		if (!readToken (token))
		{
			eof = true;
			goto out;
		}
		if (isType (token, TOKEN_IDENTIFIER)){
			vStringCat (currentid, token->string);
			vStringStripTrailing (currentid);
			if (vStringLength (currentid) > 0)
			{
				vStringCopy (name->string, currentid);
				makeBibTag (name, kind);
			}
		}
		else
		{ // should find an identifier for bib item at first place
			eof = true;
			goto out;
		}
	}

 out:
	deleteToken (name);
	vStringDelete (currentid);
	return eof;
}

static void parseBibFile (tokenInfo *const token)
{
	bool eof = false;

	do
	{
		if (!readToken (token))
			break;

		if (isType (token, TOKEN_KEYWORD))
		{
			switch (token->keyword)
			{
				case KEYWORD_article:
					eof = parseTag (token, BIBTAG_ARTICLE);
					break;
				case KEYWORD_book:
					eof = parseTag (token, BIBTAG_BOOK);
					break;
				case KEYWORD_booklet:
					eof = parseTag (token, BIBTAG_BOOKLET);
					break;
				case KEYWORD_conference:
					eof = parseTag (token, BIBTAG_CONFERENCE);
					break;
				case KEYWORD_inbook:
					eof = parseTag (token, BIBTAG_INBOOK);
					break;
				case KEYWORD_incollection:
					eof = parseTag (token, BIBTAG_INCOLLECTION);
					break;
				case KEYWORD_inproceedings:
					eof = parseTag (token, BIBTAG_INPROCEEDINGS);
					break;
				case KEYWORD_manual:
					eof = parseTag (token, BIBTAG_MANUAL);
					break;
				case KEYWORD_mastersthesis:
					eof = parseTag (token, BIBTAG_MASTERSTHESIS);
					break;
				case KEYWORD_misc:
					eof = parseTag (token, BIBTAG_MISC);
					break;
				case KEYWORD_phdthesis:
					eof = parseTag (token, BIBTAG_PHDTHESIS);
					break;
				case KEYWORD_proceedings:
					eof = parseTag (token, BIBTAG_PROCEEDINGS);
					break;
				case KEYWORD_string:
					eof = parseTag (token, BIBTAG_STRING);
					break;
				case KEYWORD_techreport:
					eof = parseTag (token, BIBTAG_TECHREPORT);
					break;
				case KEYWORD_unpublished:
					eof = parseTag (token, BIBTAG_UNPUBLISHED);
					break;
				default:
					break;
			}
		}
		if (eof)
			break;
	} while (true);
}

static void initialize (const langType language)
{
	Lang_bib = language;
}

static void findBibTags (void)
{
	tokenInfo *const token = newToken ();

	parseBibFile (token);

	deleteToken (token);
}

/* Create parser definition structure */
extern parserDefinition* BibtexParser (void)
{
	Assert (ARRAY_SIZE (BibKinds) == BIBTAG_COUNT);
	static const char *const extensions [] = { "bib", NULL };
	parserDefinition *const def = parserNew ("BibTeX");
	def->extensions = extensions;
	/*
	 * New definitions for parsing instead of regex
	 */
	def->kindTable		= BibKinds;
	def->kindCount		= ARRAY_SIZE (BibKinds);
	def->parser				= findBibTags;
	def->initialize		= initialize;
	def->keywordTable	= BibKeywordTable;
	def->keywordCount	= ARRAY_SIZE (BibKeywordTable);
	return def;
}
