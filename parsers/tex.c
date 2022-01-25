/*
 *	 Copyright (c) 2008, David Fishburn
 *	 Copyright (c) 2012, Jan Larres
 *
 *	 This source code is released for free distribution under the terms of the
 *	 GNU General Public License version 2 or (at your option) any later version.
 *
 *	 This module contains functions for generating tags for TeX language files.
 *
 *	 Tex language reference:
 *		 http://en.wikibooks.org/wiki/TeX#The_Structure_of_TeX
 */

/*
 *	 INCLUDE FILES
 */
#include "general.h"	/* must always come first */
#include <ctype.h>	/* to define isalpha () */
#ifdef DEBUG
#include <stdio.h>
#endif
#include <string.h>

#include "debug.h"
#include "entry.h"
#include "keyword.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

#include "tex.h"

/*
 *	 MACROS
 */
#define isType(token,t)		(bool) ((token)->type == (t))
#define isKeyword(token,k)	(bool) ((token)->keyword == (k))
#define isIdentChar(c) \
	(isalpha (c) || isdigit (c) || (c) >= 0x80 || (c) == '$' || \
		(c) == '_' || (c) == '#' || (c) == '-' || (c) == '.' || (c) == ':')

/*
 *	 DATA DECLARATIONS
 */

/*
 * Used to specify type of keyword.
 */
enum eKeywordId {
	KEYWORD_part,
	KEYWORD_chapter,
	KEYWORD_section,
	KEYWORD_subsection,
	KEYWORD_subsubsection,
	KEYWORD_paragraph,
	KEYWORD_subparagraph,
	KEYWORD_label,
	KEYWORD_include,
	KEYWORD_input,
	KEYWORD_begin,
	KEYWORD_end,
	KEYWORD_bibitem,
	KEYWORD_bibliography,
	KEYWORD_newcommand,
	KEYWORD_renewcommand,
	KEYWORD_providecommand,
	KEYWORD_def,
	KEYWORD_declaremathoperator,
	KEYWORD_newenvironment,
	KEYWORD_renewenvironment,
	KEYWORD_newtheorem,
	KEYWORD_newcounter,
};
typedef int keywordId; /* to allow KEYWORD_NONE */

enum eTokenType {
	/* 0..255 are the byte's value.  Some are named for convenience */
	TOKEN_OPEN_PAREN = '(',
	TOKEN_CLOSE_PAREN = ')',
	TOKEN_OPEN_CURLY = '{',
	TOKEN_CLOSE_CURLY = '}',
	TOKEN_OPEN_SQUARE = '[',
	TOKEN_CLOSE_SQUARE = ']',
	TOKEN_STAR = '*',
	/* above is special types */
	TOKEN_UNDEFINED = 256,
	TOKEN_KEYWORD,
	TOKEN_IDENTIFIER,
	TOKEN_STRING,
};
typedef int tokenType;

typedef struct sTokenInfo {
	tokenType		type;
	keywordId		keyword;
	vString *		string;
	vString *		scope;
	unsigned long 	lineNumber;
	MIOPos 			filePosition;
} tokenInfo;

/*
 *	DATA DEFINITIONS
 */

static langType Lang_tex;

static vString *lastPart;
static vString *lastChapter;
static vString *lastSection;
static vString *lastSubS;
static vString *lastSubSubS;

typedef enum {
	TEXTAG_PART,
	TEXTAG_CHAPTER,
	TEXTAG_SECTION,
	TEXTAG_SUBSECTION,
	TEXTAG_SUBSUBSECTION,
	TEXTAG_PARAGRAPH,
	TEXTAG_SUBPARAGRAPH,
	TEXTAG_LABEL,
	TEXTAG_XINPUT,
	TEXTAG_BIBITEM,
	TEXTAG_COMMAND,
	TEXTAG_OPERATOR,
	TEXTAG_ENVIRONMENT,
	TEXTAG_THEOREM,
	TEXTAG_COUNTER,
	TEXTAG_COUNT
} texKind;

typedef enum {
	TEX_XINPUT_INCLUDED,
	TEX_XINPUT_INPUT,
	TEX_XINPUT_BIBLIOGRAPHY,
} texInputRole;

typedef enum {
	TEX_ENVIRONMENT_USED,
} texEnvironmentRole;

static roleDefinition TexInputRoles [] = {
	{ true, "included",
	  "external input file specified with \\include" },
	{ true, "input",
	  "external input file specified with \\input" },
	{ true, "bibliography",
	  "bibliography (.bib) file" },
};

static roleDefinition TexEnvironmentRoles [] = {
	{ false, "used", "environment usage introduced by \\begin{MyEnv}" },
};

static kindDefinition TexKinds [] = {
	{ true,  'p', "part",			  "parts"			   },
	{ true,  'c', "chapter",		  "chapters"		   },
	{ true,  's', "section",		  "sections"		   },
	{ true,  'u', "subsection",		  "subsections"		   },
	{ true,  'b', "subsubsection",	  "subsubsections"	   },
	{ true,  'P', "paragraph",		  "paragraphs"		   },
	{ true,  'G', "subparagraph",	  "subparagraphs"	   },
	{ true,  'l', "label",			  "labels"			   },
	{ true,  'i', "xinput",			  "external input files",
	  .referenceOnly = true, ATTACH_ROLES(TexInputRoles)   },
	{ true,  'B', "bibitem",		  "bibliography items" },
	{ true,  'C', "command",		  "command created with \\newcommand" },
	{ true,  'o', "operator",		  "math operator created with \\DeclareMathOperator" },
	{ true,  'e', "environment",	  "environment created with \\newenvironment",
	  .referenceOnly = false, ATTACH_ROLES(TexEnvironmentRoles) },
	{ true,  't', "theorem",		  "theorem created with \\newtheorem" },
	{ true,  'N', "counter",		  "counter created with \\newcounter" },
};

static const keywordTable TexKeywordTable [] = {
	/* keyword			keyword ID */
	{ "part",			KEYWORD_part				},
	{ "chapter",		KEYWORD_chapter				},
	{ "section",		KEYWORD_section				},
	{ "subsection",		KEYWORD_subsection			},
	{ "subsubsection",	KEYWORD_subsubsection		},
	{ "paragraph",		KEYWORD_paragraph			},
	{ "subparagraph",	KEYWORD_subparagraph		},
	{ "label",			KEYWORD_label				},
	{ "include",		KEYWORD_include				},
	{ "input",			KEYWORD_input				},
	{ "begin",			KEYWORD_begin				},
	{ "end",			KEYWORD_end					},
	{ "bibitem",		KEYWORD_bibitem				},
	{ "bibliography",	KEYWORD_bibliography		},
	{ "newcommand",		KEYWORD_newcommand			},
	{ "renewcommand",	KEYWORD_renewcommand		},
	{ "providecommand",	KEYWORD_providecommand		},
	{ "def",			KEYWORD_def					},
	{ "DeclareMathOperator",	KEYWORD_declaremathoperator	},
	{ "newenvironment",	KEYWORD_newenvironment		},
	{ "renewenvironment",	KEYWORD_renewenvironment},
	{ "newtheorem",		KEYWORD_newtheorem			},
	{ "newcounter",		KEYWORD_newcounter			},
};

/*
 * FUNCTION DECLARATIONS
 */

static bool notifyReadingIdentifier  (tokenInfo *id_token, bool *tokenUnprocessed);
static bool notifyReadingBeginEnvironment (tokenInfo *token, vString *envName, bool *tokenUnprocessed);
static bool notifyReadingEndEnvironment (vString *envName);


/*
 *	 FUNCTION DEFINITIONS
 */

static tokenInfo *newToken (void)
{
	tokenInfo *const token = xMalloc (1, tokenInfo);

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	token->string		= vStringNew ();
	token->scope		= vStringNew ();
	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	return token;
}

static void deleteToken (tokenInfo *const token)
{
	vStringDelete (token->string);
	vStringDelete (token->scope);
	eFree (token);
}

static int getScopeInfo(texKind kind, vString *const parentName)
{
	int parentKind = KIND_GHOST_INDEX;
	int i;

	/*
	 * Put labels separately instead of under their scope.
	 * Is this The Right Thing To Do?
	 */
	if (kind >= TEXTAG_LABEL) {
		goto out;
	}

	/*
	 * This abuses the enum internals somewhat, but it should be ok in this
	 * case.
	 */
	/* TODO: This loop and conditions can be squashed. */
	for (i = kind - 1; i >= TEXTAG_PART; --i) {
		if (i == TEXTAG_SUBSECTION && vStringLength(lastSubS) > 0) {
			parentKind = i;
			break;
		} else if (i == TEXTAG_SECTION && vStringLength(lastSection) > 0) {
			parentKind = i;
			break;
		} else if (i == TEXTAG_CHAPTER && vStringLength(lastChapter) > 0) {
			parentKind = i;
			break;
		} else if (i == TEXTAG_PART && vStringLength(lastPart) > 0) {
			parentKind = i;
			break;
		}
	}

	/*
	 * Is '""' the best way to separate scopes? It has to be something that
	 * should ideally never occur in normal LaTeX text.
	 */
	for (i = TEXTAG_PART; i < (int)kind; ++i) {
		if (i == TEXTAG_PART && vStringLength(lastPart) > 0) {
			vStringCat(parentName, lastPart);
		} else if (i == TEXTAG_CHAPTER && vStringLength(lastChapter) > 0) {
			if (vStringLength(parentName) > 0) {
				vStringCatS(parentName, "\"\"");
			}
			vStringCat(parentName, lastChapter);
		} else if (i == TEXTAG_SECTION && vStringLength(lastSection) > 0) {
			if (vStringLength(parentName) > 0) {
				vStringCatS(parentName, "\"\"");
			}
			vStringCat(parentName, lastSection);
		} else if (i == TEXTAG_SUBSECTION && vStringLength(lastSubS) > 0) {
			if (vStringLength(parentName) > 0) {
				vStringCatS(parentName, "\"\"");
			}
			vStringCat(parentName, lastSubS);
		}
	}
 out:
	return parentKind;
}

/*
 *	 Tag generation functions
 */

struct symbolData {
	langType lang;
	int kind;
	int *corkQueue;
};

static bool findTheName (int corkIndex, tagEntryInfo *entry, void *data)
{
	struct symbolData *symbolData = data;

	if (entry->langType == symbolData->lang && entry->kindIndex == symbolData->kind)
	{
		/* TODO: The case operation should be removed */
		*symbolData->corkQueue = corkIndex;
		return false;
	}
	return true;
}

static int makeTexTag (tokenInfo *const token, int kind,
					   int roleIndex, bool unique, int scopeIndex)
{
	int corkQueue = CORK_NIL;
	const char *const name = vStringValue (token->string);

	if (unique)
	{
		struct symbolData data = {
			.lang = getInputLanguage(),
			.kind = kind,
			.corkQueue = &corkQueue,
		};
		/* TODO: The case operation should be removed */
		if (foreachEntriesInScope (scopeIndex, name, findTheName, (void *)&data) == false)
			return *data.corkQueue;
	}

	tagEntryInfo e;
	initTagEntry (&e, name, kind);

	e.lineNumber   = token->lineNumber;
	e.filePosition = token->filePosition;

	vString *parentName = NULL;


	if (unique)
		e.extensionFields.scopeIndex = scopeIndex;

	/* Filling e.extensionFields.scopeKindIndex and
	 * e.extensionFields.scopeName can be filled from "kind" parameter
	 * of this function only when Tex parser calls this function. The
	 * fields cannot be filled with a kind defined in a subparser.
	 * Subparsers may fill the scope after running strategy. So in the
	 * context of a subparser, filling the scope fields here is not
	 * needed.
	 */
	if (Lang_tex == getInputLanguage ())
	{
		int parentKind = KIND_GHOST_INDEX;
		parentName = vStringNew();
		parentKind = getScopeInfo(kind, parentName);
		if (parentKind != KIND_GHOST_INDEX) {
			e.extensionFields.scopeKindIndex = parentKind;
			e.extensionFields.scopeName = vStringValue(parentName);
		}
	}

	assignRole (&e, roleIndex);

	corkQueue = makeTagEntry (&e);
	vStringDelete (parentName);	/* NULL is o.k. */

	if (unique && corkQueue != CORK_NIL)
		registerEntry (corkQueue);

	return corkQueue;
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

static bool readTokenFull (tokenInfo *const token, const bool includeWhitespaces)
{
	int c;
	int whitespaces = -1;

	token->type			= TOKEN_UNDEFINED;
	token->keyword		= KEYWORD_NONE;
	vStringClear (token->string);

getNextChar:

	do
	{
		c = getcFromInputFile ();
		whitespaces++;
	}
	while (c == '\t'  ||  c == ' ' ||  c == '\n');

	token->lineNumber   = getInputLineNumber ();
	token->filePosition = getInputFilePosition ();

	if (includeWhitespaces && whitespaces > 0 && c != '%' && c != EOF)
	{
		ungetcToInputFile (c);
		c = ' ';
	}

	token->type = (unsigned char) c;
	switch (c)
	{
		case EOF: return false;

		case '\\':
				  /*
				   * All Tex tags start with a backslash.
				   * Check if the next character is an alpha character
				   * else it is not a potential tex tag.
				   */
				  c = getcFromInputFile ();
				  if (! isalpha (c))
					  ungetcToInputFile (c);
				  else
				  {
					  vStringPut (token->string, '\\');
					  parseIdentifier (token->string, c);
					  token->keyword = lookupKeyword (vStringValue (token->string) + 1, Lang_tex);
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

static bool readToken (tokenInfo *const token)
{
	return readTokenFull (token, false);
}

static void copyToken (tokenInfo *const dest, tokenInfo *const src)
{
	dest->lineNumber = src->lineNumber;
	dest->filePosition = src->filePosition;
	dest->type = src->type;
	dest->keyword = src->keyword;
	vStringCopy (dest->string, src->string);
	vStringCopy (dest->scope, src->scope);
}

static void updateScopeInfo (texKind kind, vString *fullname)
{
	switch (kind)
	{
		case TEXTAG_PART:
			vStringCopy(lastPart, fullname);
			vStringClear(lastChapter);
			vStringClear(lastSection);
			vStringClear(lastSubS);
			vStringClear(lastSubSubS);
			break;
		case TEXTAG_CHAPTER:
			vStringCopy(lastChapter, fullname);
			vStringClear(lastSection);
			vStringClear(lastSubS);
			vStringClear(lastSubSubS);
			break;
		case TEXTAG_SECTION:
			vStringCopy(lastSection, fullname);
			vStringClear(lastSubS);
			vStringClear(lastSubSubS);
			break;
		case TEXTAG_SUBSECTION:
			vStringCopy(lastSubS, fullname);
			vStringClear(lastSubSubS);
			break;
		case TEXTAG_SUBSUBSECTION:
			vStringCopy(lastSubSubS, fullname);
			break;
		default:
			break;
	}
}

/*
 *	 Scanning functions
 */

/* STRATEGY array represents the sequence of * expected tokens. If an
 * input token matches the current * expectation (the current strategy),
 * parseWithStrategy() runs * the actions attached to the strategy.
 *
 * The actions are making a tag with the kind specified with kindIndex
 * field of the current strategy and/or storing a name to NAME field
 * of the current strategy.
 *
 * If the input token doesn't much the current strategy, above actions
 * are not run. If TEX_NAME_FLAG_OPTIONAL is specified in FLAGS field
 * of the current specified, parseWithStrategy() tries the next
 * strategy of STRATEGY array without reading a new token.  If
 * TEX_NAME_FLAG_OPTIONAL is not in FLAGS field, parseWithStrategy()
 * returns the control to its caller immediately.
 *
 * TOKENUNPROCESSED is used for both input and output.  As input,
 * TOKENUNPROCESSED tells whether parseWithStrategy() should read a
 * new token before matching the STRATEGY array or not.  If
 * TOKENUNPROCESSED is true, parseWithStrategy function reads a new
 * token before matching.  As output, TOKENUNPROCESSED tells the
 * caller of parseWithStrategy() that a new token is already stored to
 * TOKEN but parseWithStrategy() has not processed yet.
 */
static bool parseWithStrategy (tokenInfo *token,
							   struct TexParseStrategy *strategy,
							   bool *tokenUnprocessed)
{
	bool next_token = !*tokenUnprocessed;
	tokenInfo * name = NULL;
	bool eof = false;
	bool exclusive = false;

	for (struct TexParseStrategy *s = strategy; s->type != 0; ++s)
		s->corkIndex = CORK_NIL;

	for (struct TexParseStrategy *s = strategy; !eof && s->type != 0; ++s)
	{
		if (s->kindIndex != KIND_GHOST_INDEX || s->name)
		{
			name = newToken ();
			break;
		}
	}

	for (struct TexParseStrategy *s = strategy; !eof && s->type != 0; ++s)
	{
		bool capture_name = s->kindIndex != KIND_GHOST_INDEX || s->name;

		if (next_token)
		{
			if (!readToken (token))
			{
				eof = true;
				break;
			}
		}

		if ((s->type == '<' && isType (token, '<'))
			|| (s->type == '[' && isType (token, '[')))
		{
			tokenType terminator = (s->type == '<') ? '>' : ']';

			next_token = true;


			if (!readToken (token))
			{
				eof = true;
				break;
			}
			if (capture_name)
			{
				copyToken (name, token);
				vStringClear (name->string);
			}

			while (! isType (token, terminator))
			{
				if (capture_name && isType (token, TOKEN_IDENTIFIER))
				{
					if (vStringLength (name->string) > 0)
						vStringPut (name->string, ' ');
					vStringCat (name->string, token->string);
				}

				if (!readTokenFull (token,
									s->flags & TEX_NAME_FLAG_INCLUDING_WHITESPACE))
				{
					eof = true;
					break;
				}
			}
			if (!exclusive && capture_name && vStringLength (name->string) > 0)
			{
				if (s->kindIndex != KIND_GHOST_INDEX)
					s->corkIndex = makeTexTag (name, s->kindIndex, s->roleIndex,
											   s->unique, s->scopeIndex);

				if (s->name)
					vStringCopy(s->name, name->string);

				if (s->flags & TEX_NAME_FLAG_EXCLUSIVE)
					exclusive = true;
			}
		}
		else if (s->type == '*' && isType (token, '*'))
			next_token = true;
		else if (((s->type == '{' || s->type == '\\') && isType (token, '{')) ||
			(s->type == '\\' && isType (token, TOKEN_IDENTIFIER)))
		{
			int depth = 1;
			bool missing_parens = isType (token, TOKEN_IDENTIFIER);

			next_token = true;

			if (!missing_parens && !readToken (token))
			{
				eof = true;
				break;
			}
			if (capture_name)
			{
				copyToken (name, token);
				vStringClear (name->string);
			}
			if (missing_parens)
			{
				vStringCat (name->string, token->string);
				depth = 0;
			}

			/* Handle the case the code like \section{} */
			if (isType (token, '}'))
				break;
			while (depth > 0)
			{
				if (capture_name)
				{
					if (isType (token, TOKEN_IDENTIFIER) || isType (token, TOKEN_KEYWORD))
						vStringCat (name->string, token->string);
					else
						vStringPut (name->string, token->type);
				}
				if (!readTokenFull (token,
									s->flags & TEX_NAME_FLAG_INCLUDING_WHITESPACE))
				{
					eof = true;
					break;
				}
				else if (isType (token, TOKEN_OPEN_CURLY))
					depth++;
				else if (isType (token, TOKEN_CLOSE_CURLY))
					depth--;
			}
			if (!exclusive && depth == 0 && capture_name && vStringLength (name->string) > 0)
			{
				vStringStripTrailing (name->string);

				if (s->kindIndex != KIND_GHOST_INDEX)
					s->corkIndex = makeTexTag (name, s->kindIndex, s->roleIndex,
											   s->unique, s->scopeIndex);

				if (s->name)
					vStringCopy(s->name, name->string);

				if (s->flags & TEX_NAME_FLAG_EXCLUSIVE)
					exclusive = true;

			}
		}
		else if (s->flags & TEX_NAME_FLAG_OPTIONAL)
			/* Apply next strategy to the same token */
			next_token = false;
		else
		{
			*tokenUnprocessed = true;
			break;
		}
	}

	/* The last token is optional and not present - let the caller know */
	if (!next_token)
		*tokenUnprocessed = true;

	if (name)
		deleteToken (name);

	return eof;
}

static bool parseTagFull (tokenInfo *const token, texKind kind, int roleIndex, bool enterSquare, bool *tokenUnprocessed)
{
	bool eof = false;
	vString *taggedName = vStringNew();

	/*
	 * Tex tags are of these formats:
	 *   \keyword{any number of words}
	 *   \keyword[short desc]{any number of words}
	 *   \keyword*[short desc]{any number of words}
	 *
	 * When a keyword is found, loop through all words within
	 * the curly braces for the tag name.
	 *
	 * If the keyword is label like \label, words in the square
	 * brackets should be skipped. This can be controlled
	 * with `enterSquare' parameter; true is for tagging, and
	 * false is for skipping.
	 */

	struct TexParseStrategy strategy [] = {
		{
			.type = '[',
			.flags = TEX_NAME_FLAG_OPTIONAL,
			/* .kindIndex is initialized dynamically. */
		},
		{
			.type = '*',
			.flags = TEX_NAME_FLAG_OPTIONAL,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = '{',
			.flags = TEX_NAME_FLAG_INCLUDING_WHITESPACE,
			.kindIndex = kind,
			.roleIndex = roleIndex,
			.name = taggedName,
			.unique = false,
		},
		{
			.type = 0
		}
	};


	if (enterSquare)
	{
		strategy [0].kindIndex = kind;
		strategy [0].roleIndex = roleIndex;
		strategy [0].flags |= TEX_NAME_FLAG_EXCLUSIVE;
		strategy [0].name = taggedName;
		strategy [0].unique = false;
	}
	else
	{
		strategy [0].kindIndex = KIND_GHOST_INDEX;
		strategy [0].name = NULL;
	}

	if (parseWithStrategy (token, strategy, tokenUnprocessed))
	{
		eof = true;
		goto out;
	}

	/*
	 * save the name of the last section definitions for scope-resolution
	 * later
	 */
	if (vStringLength (taggedName) > 0)
		updateScopeInfo (kind, taggedName);

 out:
	vStringDelete (taggedName);

	return eof;
}

static bool parseTag (tokenInfo *const token, texKind kind,
					  bool enterSquare, bool *tokenUnprocessed)
{
	return parseTagFull (token, kind, ROLE_DEFINITION_INDEX,
						 enterSquare, tokenUnprocessed);
}

static bool parseEnv (tokenInfo *const token, bool begin, bool *tokenUnprocessed)
{
	bool eof = false;
	vString *envName = vStringNew ();
	struct TexParseStrategy strategy [] = {
		{
			.type = '{',
			.flags = TEX_NAME_FLAG_INCLUDING_WHITESPACE,
			.kindIndex = begin ? TEXTAG_ENVIRONMENT : KIND_GHOST_INDEX,
			.roleIndex = TEX_ENVIRONMENT_USED,
			.name = envName,
		},
		{
			.type = 0
		}
	};

	if (parseWithStrategy (token, strategy, tokenUnprocessed))
	{
		eof = true;
		goto out;
	}


	if (vStringLength (envName) > 0)
	{
		if (begin)
			eof = notifyReadingBeginEnvironment (token, envName, tokenUnprocessed);
		else
			eof = notifyReadingEndEnvironment (envName);
	}

 out:
	vStringDelete (envName);

	return eof;

}

static bool parseNewcommandFull (tokenInfo *const token, bool *tokenUnprocessed, texKind kind)
{
	bool eof = false;

	/* \newcommand{cmd}[args][opt]{def} */
	/* \newcommand\cmd[args][opt]{def} */
	/* \def\cmd{replacement} */
	struct TexParseStrategy strategy [] = {
		{
			.type = '\\',
			.flags = 0,
			.kindIndex = kind,
			.roleIndex = ROLE_DEFINITION_INDEX,
			.name = NULL,
			.unique = false,
		},
		{
			.type = '[',
			.flags = TEX_NAME_FLAG_OPTIONAL,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = '[',
			.flags = TEX_NAME_FLAG_OPTIONAL,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = '{',
			.flags = 0,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = 0
		}
	};

	if (parseWithStrategy (token, strategy, tokenUnprocessed))
		eof = true;

	return eof;
}

static bool parseNewcommand (tokenInfo *const token, bool *tokenUnprocessed)
{
	return parseNewcommandFull (token, tokenUnprocessed, TEXTAG_COMMAND);
}

static bool parseNewEnvironment (tokenInfo *const token, bool *tokenUnprocessed)
{
	bool eof = false;
	/* \newenvironment{nam}[args]{begdef}{enddef} */
	struct TexParseStrategy strategy [] = {
		{
			.type = '{',
			.flags = 0,
			.kindIndex = TEXTAG_ENVIRONMENT,
			.roleIndex = ROLE_DEFINITION_INDEX,
			.name = NULL,
			.unique = false,
		},
		{
			.type = '[',
			.flags = TEX_NAME_FLAG_OPTIONAL,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = '{',
			.flags = 0,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = '{',
			.flags = 0,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = 0
		}
	};

	if (parseWithStrategy (token, strategy, tokenUnprocessed))
		eof = true;

	return eof;
}

static bool parseNewTheorem (tokenInfo *const token, bool *tokenUnprocessed)
{
	bool eof = false;
	/*	\newtheorem{name}{title}
		\newtheorem{name}{title}[numbered_within]
		\newtheorem{name}[numbered_like]{title} */
	struct TexParseStrategy strategy [] = {
		{
			.type = '{',
			.flags = 0,
			.kindIndex = TEXTAG_THEOREM,
			.roleIndex = ROLE_DEFINITION_INDEX,
			.name = NULL,
			.unique = false,
		},
		{
			.type = '[',
			.flags = TEX_NAME_FLAG_OPTIONAL,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = '{',
			.flags = 0,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = '[',
			.flags = TEX_NAME_FLAG_OPTIONAL,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = 0
		}
	};

	if (parseWithStrategy (token, strategy, tokenUnprocessed))
		eof = true;

	return eof;
}

static bool parseNewcounter (tokenInfo *const token, bool *tokenUnprocessed)
{
	bool eof = false;
	/* \newcounter {counter}[parentCounter] */
	struct TexParseStrategy strategy [] = {
		{
			.type = '{',
			.flags = 0,
			.kindIndex = TEXTAG_COUNTER,
			.roleIndex = ROLE_DEFINITION_INDEX,
			.name = NULL,
			.unique = false,
		},
		{
			.type = '[',
			.flags = TEX_NAME_FLAG_OPTIONAL,
			.kindIndex = KIND_GHOST_INDEX,
			.name = NULL,
		},
		{
			.type = 0
		}
	};

	if (parseWithStrategy (token, strategy, tokenUnprocessed))
		eof = true;

	return eof;
}

static void parseTexFile (tokenInfo *const token)
{
	bool eof = false;
	bool tokenUnprocessed = false;

	do
	{
		if (!tokenUnprocessed)
		{
			if (!readToken (token))
				break;
		}
		tokenUnprocessed = false;

		if (isType (token, TOKEN_KEYWORD))
		{
			switch (token->keyword)
			{
				case KEYWORD_part:
					eof = parseTag (token, TEXTAG_PART, true, &tokenUnprocessed);
					break;
				case KEYWORD_chapter:
					eof = parseTag (token, TEXTAG_CHAPTER, true, &tokenUnprocessed);
					break;
				case KEYWORD_section:
					eof = parseTag (token, TEXTAG_SECTION, true, &tokenUnprocessed);
					break;
				case KEYWORD_subsection:
					eof = parseTag (token, TEXTAG_SUBSECTION, true, &tokenUnprocessed);
					break;
				case KEYWORD_subsubsection:
					eof = parseTag (token, TEXTAG_SUBSUBSECTION, true, &tokenUnprocessed);
					break;
				case KEYWORD_paragraph:
					eof = parseTag (token, TEXTAG_PARAGRAPH, true, &tokenUnprocessed);
					break;
				case KEYWORD_subparagraph:
					eof = parseTag (token, TEXTAG_SUBPARAGRAPH, true, &tokenUnprocessed);
					break;
				case KEYWORD_label:
					eof = parseTag (token, TEXTAG_LABEL, false, &tokenUnprocessed);
					break;
				case KEYWORD_include:
					eof = parseTagFull (token, TEXTAG_XINPUT, TEX_XINPUT_INCLUDED,
										false, &tokenUnprocessed);
					break;
				case KEYWORD_input:
					eof = parseTagFull (token, TEXTAG_XINPUT, TEX_XINPUT_INPUT,
										false, &tokenUnprocessed);
					break;
				case KEYWORD_begin:
					eof = parseEnv (token, true, &tokenUnprocessed);
					break;
				case KEYWORD_end:
					eof = parseEnv (token, false, &tokenUnprocessed);
					break;
				case KEYWORD_bibitem:
					eof = parseTag (token, TEXTAG_BIBITEM, false, &tokenUnprocessed);
					break;
				case KEYWORD_bibliography:
					eof = parseTagFull (token, TEXTAG_XINPUT, TEX_XINPUT_BIBLIOGRAPHY,
										false, &tokenUnprocessed);
					break;
				case KEYWORD_newcommand:
				case KEYWORD_renewcommand:
				case KEYWORD_providecommand:
				case KEYWORD_def:
					eof = parseNewcommand (token, &tokenUnprocessed);
					break;
				case KEYWORD_declaremathoperator:
					eof = parseNewcommandFull (token, &tokenUnprocessed, TEXTAG_OPERATOR);
					break;
				case KEYWORD_newenvironment:
				case KEYWORD_renewenvironment:
					eof = parseNewEnvironment (token, &tokenUnprocessed);
					break;
				case KEYWORD_newtheorem:
					eof = parseNewTheorem (token, &tokenUnprocessed);
					break;
				case KEYWORD_newcounter:
					eof = parseNewcounter (token, &tokenUnprocessed);
					break;
				default:
					break;
			}
		}
		else if (isType (token, TOKEN_IDENTIFIER))
			eof = notifyReadingIdentifier (token, &tokenUnprocessed);
		if (eof)
			break;
	} while (true);
}

static void initialize (const langType language)
{
	Assert (ARRAY_SIZE (TexKinds) == TEXTAG_COUNT);
	Lang_tex = language;

	lastPart    = vStringNew();
	lastChapter = vStringNew();
	lastSection = vStringNew();
	lastSubS    = vStringNew();
	lastSubSubS = vStringNew();
}

static void finalize (const langType language CTAGS_ATTR_UNUSED,
		      bool initialized)
{
	if (initialized)
	{
		vStringDelete(lastPart);
		lastPart = NULL;
		vStringDelete(lastChapter);
		lastChapter = NULL;
		vStringDelete(lastSection);
		lastSection = NULL;
		vStringDelete(lastSubS);
		lastSubS = NULL;
		vStringDelete(lastSubSubS);
		lastSubSubS = NULL;
	}
}

static void findTexTags (void)
{
	tokenInfo *const token = newToken ();

	parseTexFile (token);

	deleteToken (token);
}

static bool notifyReadingIdentifier (tokenInfo *id_token, bool *tokenUnprocessed)
{
	subparser *sub;
	bool eof = false;

	foreachSubparser (sub, false)
	{
		texSubparser *texsub = (texSubparser *)sub;

		if (texsub->readIdentifierNotify)
		{
			struct TexParseStrategy *strategy;

			enterSubparser(sub);

			strategy = texsub->readIdentifierNotify (texsub, id_token->string);

			if (strategy)
			{
				eof = parseWithStrategy (id_token, strategy, tokenUnprocessed);
				if (texsub->reportStrategicParsing)
					texsub->reportStrategicParsing (texsub, strategy);
			}

			leaveSubparser();

			if (strategy)
				break;
		}
	}

	return eof;
}

static bool notifyReadingBeginEnvironment (tokenInfo *token,
										   vString *envName,
										   bool *tokenUnprocessed)
{
	subparser *sub;
	bool eof = false;

	foreachSubparser (sub, false)
	{
		texSubparser *texsub = (texSubparser *)sub;

		if (texsub->readEnviromentBeginNotify)
		{
			struct TexParseStrategy *strategy;

			enterSubparser (sub);
			strategy = texsub->readEnviromentBeginNotify (texsub, envName);
			if (strategy)
			{
				eof = parseWithStrategy (token, strategy, tokenUnprocessed);
				if (texsub->reportStrategicParsing)
					texsub->reportStrategicParsing (texsub, strategy);
			}
			leaveSubparser ();
			if (strategy)
				break;
		}
	}

	return eof;
}

static bool notifyReadingEndEnvironment (vString  *envName)
{
	subparser *sub;

	foreachSubparser (sub, false)
	{
		texSubparser *texsub = (texSubparser *)sub;

		if (texsub->readEnviromentEndNotify)
		{
			bool consuming;

			enterSubparser (sub);
			consuming = texsub->readEnviromentEndNotify (texsub, envName);
			leaveSubparser ();
			if (consuming)
				break;
		}
	}

	return false;
}

/* Create parser definition structure */
extern parserDefinition* TexParser (void)
{
	static const char *const extensions [] = { "tex", NULL };
	parserDefinition *const def = parserNew ("Tex");
	def->extensions = extensions;
	/*
	 * New definitions for parsing instead of regex
	 */
	def->kindTable	= TexKinds;
	def->kindCount	= ARRAY_SIZE (TexKinds);
	def->parser		= findTexTags;
	def->initialize = initialize;
	def->finalize   = finalize;
	def->keywordTable =  TexKeywordTable;
	def->keywordCount = ARRAY_SIZE (TexKeywordTable);
	def->useCork = CORK_QUEUE  | CORK_SYMTAB;
	return def;
}
