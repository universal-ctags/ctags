/*
*   Copyright (c) 2016, Jiri Techet
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for HTML language
*   files.
*/

#include "general.h"

#include <string.h>
#include <ctype.h>

#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "keyword.h"
#include "promise.h"


typedef enum {
	K_ANCHOR,
	K_HEADING1,
	K_HEADING2,
	K_HEADING3
} htmlKind;


static kindOption HtmlKinds [] = {
	{ TRUE, 'a', "anchor",		"named anchors" },
	{ TRUE, 'h', "heading1",	"H1 headings" },
	{ TRUE, 'i', "heading2",	"H2 headings" },
	{ TRUE, 'j', "heading3",	"H3 headings" }
};

typedef enum {
	KEYWORD_h1,
	KEYWORD_h2,
	KEYWORD_h3,
	KEYWORD_a,
	KEYWORD_script,
	KEYWORD_style,
	KEYWORD_name
} keywordId;

static const keywordTable HtmlKeywordTable[] = {
	{"h1", KEYWORD_h1},
	{"h2", KEYWORD_h2},
	{"h3", KEYWORD_h3},
	{"a", KEYWORD_a},
	{"script", KEYWORD_script},
	{"style", KEYWORD_style},
	{"name", KEYWORD_name},
};

typedef enum {
	TOKEN_EOF,
	TOKEN_NAME,			/* tag and attribute names */
	TOKEN_STRING,		/* single- or double-quoted attribute value */
	TOKEN_TEXT,
	TOKEN_TAG_START,	/* <  */
	TOKEN_TAG_START2,	/* </ */
	TOKEN_TAG_END,		/* >  */
	TOKEN_TAG_END2,		/* /> */
	TOKEN_EQUAL,
	TOKEN_COMMENT,
	TOKEN_OTHER
} tokenType;

typedef struct {
	tokenType type;
	vString *string;
} tokenInfo;


static int Lang_html;


static void readTag (tokenInfo *token, vString *text);


static void readTokenText (tokenInfo *const token, boolean collectText)
{
	int c;
	int lastC = 'X';  /* whatever non-space character */

	vStringClear (token->string);

getNextChar:

	c = getcFromInputFile ();

	switch (c)
	{
		case EOF:
			token->type = TOKEN_EOF;
			break;

		case '<':
			ungetcToInputFile (c);
			token->type = TOKEN_TEXT;
			break;

		default:
			if (collectText)
			{
				if (isspace (c))
					c = ' ';
				if (c != ' ' || (c == ' ' && lastC != ' '))
				{
					if (collectText)
						vStringPut (token->string, c);
					lastC = c;
				}
			}

			goto getNextChar;
	}
}

static void readToken (tokenInfo *const token, boolean skipComments)
{
	int c;

	vStringClear (token->string);

getNextChar:

	c = getcFromInputFile ();
	while (isspace (c))
		c = getcFromInputFile ();

	switch (c)
	{
		case EOF:
			token->type = TOKEN_EOF;
			break;

		case '<':
		{
			int d = getcFromInputFile ();

			if (d == '!')
			{
				d = getcFromInputFile ();
				if (d == '-')
				{
					d = getcFromInputFile ();
					if (d == '-')
					{
						int e = ' ';
						int f = ' ';
						do
						{
							d = e;
							e = f;
							f = getcFromInputFile ();
						}
						while (f != EOF && ! (d == '-' && e == '-' && f == '>'));

						if (skipComments)
							goto getNextChar;
						else
						{
							token->type = TOKEN_COMMENT;
							break;
						}
					}
				}
				ungetcToInputFile (d);
				token->type = TOKEN_OTHER;
			}
			else if (d == '?')
				token->type = TOKEN_OTHER;
			else if (d == '/')
				token->type = TOKEN_TAG_START2;
			else
			{
				ungetcToInputFile (d);
				token->type = TOKEN_TAG_START;
			}
			break;
		}
		case '/':
		{
			int d = getcFromInputFile ();
			if (d == '>')
				token->type = TOKEN_TAG_END2;
			else
			{
				ungetcToInputFile (d);
				token->type = TOKEN_OTHER;
			}
			break;
		}
		case '>':
			token->type = TOKEN_TAG_END;
			break;

		case '=':
			token->type = TOKEN_EQUAL;
			break;

		case '"':
		case '\'':
		{
			const int delimiter = c;
			c = getcFromInputFile ();
			while (c != EOF && c != delimiter)
			{
				vStringPut (token->string, c);
				c = getcFromInputFile ();
			}
			token->type = TOKEN_STRING;
			break;
		}

		default:
		{
			do
			{
				vStringPut (token->string, tolower (c));
				c = getcFromInputFile ();
			}
			while (!isspace (c) && c != '<' && c != '>' && c != '/' &&
				   c != '=' && c != '\'' && c != '"' && c != EOF);
			if (c != EOF)
				ungetcToInputFile (c);
			token->type = TOKEN_NAME;
			break;
		}
	}
}

static void appendText (vString *text, vString *appendedText)
{
	if (text != NULL && vStringLength (appendedText) > 0)
	{
		if (vStringLength (text) > 0 && vStringLast (text) == ' ' &&
			vStringLength (appendedText) > 0 && vStringChar (appendedText, 0) == ' ')
		{
			vStringStripTrailing (text);
		}
		vStringCat (text, appendedText);
	}
}

static boolean readTagContent (tokenInfo *token, vString *text, long *line, int *lineOffset)
{
	tokenType type;

	readTokenText (token, text != NULL);
	appendText (text, token->string);

	do
	{
		*line = getInputLineNumber ();
		*lineOffset = getInputLineOffset ();
		readToken (token, FALSE);
		type = token->type;
		if (type == TOKEN_TAG_START)
			readTag (token, text);
		if (type == TOKEN_COMMENT || type == TOKEN_TAG_START)
		{
			readTokenText (token, text != NULL);
			appendText (text, token->string);
		}
	}
	while (type == TOKEN_COMMENT || type == TOKEN_TAG_START);

	return type == TOKEN_TAG_START2;
}

static void readTag (tokenInfo *token, vString *text)
{
	boolean textCreated = FALSE;

	readToken (token, TRUE);
	if (token->type == TOKEN_NAME)
	{
		keywordId startTag;
		boolean isHeading;

		startTag = lookupKeyword (vStringValue (token->string), Lang_html);
		isHeading = (startTag == KEYWORD_h1 || startTag == KEYWORD_h2 || startTag == KEYWORD_h3);
		if (text == NULL && isHeading)
		{
			text = vStringNew ();
			textCreated = TRUE;
		}

		do
		{
			readToken (token, TRUE);
			if (startTag == KEYWORD_a && token->type == TOKEN_NAME)
			{
				keywordId attribute = lookupKeyword (vStringValue (token->string), Lang_html);

				if (attribute == KEYWORD_name)
				{
					readToken (token, TRUE);
					if (token->type == TOKEN_EQUAL)
					{
						readToken (token, TRUE);
						if (token->type == TOKEN_STRING || token->type == TOKEN_NAME)
							makeSimpleTag (token->string, HtmlKinds, K_ANCHOR);
					}
				}
			}
		}
		while (token->type != TOKEN_TAG_END && token->type != TOKEN_TAG_END2 &&
			   token->type != TOKEN_EOF);

		if (token->type == TOKEN_TAG_END)
		{
			long startSourceLineNumber = getSourceLineNumber ();
			long startLineNumber = getInputLineNumber ();
			int startLineOffset = getInputLineOffset ();
			long endLineNumber;
			int endLineOffset;
			boolean tag_start2;

			tag_start2 = readTagContent (token, text, &endLineNumber, &endLineOffset);

			if (tag_start2)
			{
				readToken (token, TRUE);
				if (isHeading && textCreated && vStringLength (text) > 0)
				{
					keywordId endTag = lookupKeyword (vStringValue (token->string), Lang_html);
					if (startTag == endTag)
					{
						htmlKind headingKind;

						if (startTag == KEYWORD_h1)
							headingKind = K_HEADING1;
						else if (startTag == KEYWORD_h2)
							headingKind = K_HEADING2;
						else
							headingKind = K_HEADING3;

						vStringStripTrailing (text);
						makeSimpleTag (text, HtmlKinds, headingKind);
					}
				}
				else if (startTag == KEYWORD_script)
				{
					keywordId endTag = lookupKeyword (vStringValue (token->string), Lang_html);
					if (startTag == endTag)
						makePromise ("JavaScript", startLineNumber, startLineOffset,
									 endLineNumber, endLineOffset, startSourceLineNumber);
				}
				else if (startTag == KEYWORD_style)
				{
					keywordId endTag = lookupKeyword (vStringValue (token->string), Lang_html);
					if (startTag == endTag)
						makePromise ("CSS", startLineNumber, startLineOffset,
									 endLineNumber, endLineOffset, startSourceLineNumber);
				}

				readToken (token, TRUE);
			}
		}
	}

	if (textCreated)
		vStringDelete (text);
}

static void findHtmlTags (void)
{
	tokenInfo token;

	token.string = vStringNew ();

	do
	{
		readToken (&token, TRUE);
		if (token.type == TOKEN_TAG_START)
			readTag (&token, NULL);
	}
	while (token.type != TOKEN_EOF);

	vStringDelete (token.string);
}

static void initialize (const langType language)
{
	Lang_html = language;
}

/* parser definition */
extern parserDefinition* HtmlParser (void)
{
	static const char *const extensions [] = { "htm", "html", NULL };
	parserDefinition* def = parserNew ("HTML");
	def->kinds        = HtmlKinds;
	def->kindCount    = ARRAY_SIZE (HtmlKinds);
	def->extensions   = extensions;
	def->parser       = findHtmlTags;
	def->initialize   = initialize;
	def->keywordTable = HtmlKeywordTable;
	def->keywordCount = ARRAY_SIZE (HtmlKeywordTable);
	return def;
}
