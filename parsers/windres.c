/*
 *	windres.c
 *
 *	Copyright (c) 2013, Frank Fesevur <ffes(at)users.sourceforge.net>
 *
 *	This source code is released for free distribution under the terms of the
 *	GNU General Public License version 2 or (at your option) any later version.
 *
 *	This module contains functions for generating tags for Windows Resource files.
 */

#include "general.h"

#include <string.h>
#include <ctype.h>

#include "parse.h"
#include "read.h"
#include "routines.h"

static int _blockDepth = 0;

typedef enum _WindResKinds
{
	K_NONE = -1,
	K_DIALOG,
	K_MENU,
	K_ICON,
	K_BITMAP,
	K_CURSOR,
	K_FONT,
	K_VERSION,
	K_ACCELERATORS
} ResKind;

static kindDefinition ResKinds [] = {
	{ true, 'd', "dialog",			"dialogs"		},
	{ true, 'm', "menu",			"menus"			},
	{ true, 'i', "icon",			"icons"			},
	{ true, 'b', "bitmap",			"bitmaps"		},
	{ true, 'c', "cursor",			"cursors"		},
	{ true, 'f', "font",			"fonts"			},
	{ true, 'v', "version",			"versions"		},
	{ true, 'a', "accelerators",	"accelerators"	}
};

typedef enum _WindResParserState
{
	P_STATE_NONE,
	P_STATE_IN_COMMENT,
	P_STATE_IN_STATEMENTS,		/* Between first line of item and BEGIN/{ */
	P_STATE_IN_BLOCK,			/* In a BEGIN/END or {/} block */
	P_STATE_AT_END
} ResParserState;

static void makeResTag(vString *name, ResKind kind)
{
	vStringStripTrailing(name);
	makeSimpleTag(name, kind);
	vStringClear(name);
}

static ResParserState parseResDefinition(const unsigned char *line)
{
	ResParserState state = P_STATE_NONE;
	vString *name, *type;

	name = vStringNew();
	while (*line && !isspace((int) *line))
	{
		vStringPut(name, (int) *line);
		line++;
	}

	while (*line && isspace((int) *line))
		line++;

	type = vStringNew();
	while (*line && !isspace((int) *line))
	{
		vStringPut(type, (int) *line);
		line++;
	}

	if (strcmp(vStringValue(type), "DIALOG") == 0 || strcmp(vStringValue(type), "DIALOGEX") == 0)
	{
		makeResTag(name, K_DIALOG);
		state = P_STATE_IN_STATEMENTS;
	}
	else if (strcmp(vStringValue(type), "MENU") == 0 || strcmp(vStringValue(type), "MENUEX") == 0)
	{
		makeResTag(name, K_MENU);
		state = P_STATE_IN_STATEMENTS;
	}
	else if (strcmp(vStringValue(type), "VERSIONINFO") == 0)
	{
		makeResTag(name, K_VERSION);
		state = P_STATE_IN_STATEMENTS;
	}
	else if (strcmp(vStringValue(type), "ACCELERATORS") == 0)
	{
		makeResTag(name, K_ACCELERATORS);
		state = P_STATE_IN_STATEMENTS;
	}
	else if (strcmp(vStringValue(type), "ICON") == 0)
	{
		makeResTag(name, K_ICON);
		state = P_STATE_NONE;
	}
	else if (strcmp(vStringValue(type), "CURSOR") == 0)
	{
		makeResTag(name, K_CURSOR);
		state = P_STATE_NONE;
	}
	else if (strcmp(vStringValue(type), "BITMAP") == 0)
	{
		makeResTag(name, K_BITMAP);
		state = P_STATE_NONE;
	}
	else if (strcmp(vStringValue(type), "FONT") == 0)
	{
		makeResTag(name, K_FONT);
		state = P_STATE_NONE;
	}

	vStringDelete(name);
	vStringDelete(type);

	return state;
}

static ResParserState parseResLine(const unsigned char *line, ResParserState state)
{
	while (*line != '\0')	/* readLineFromInputFile returns NULL terminated strings */
	{
		while (isspace((int) *line))
			line++;

		switch (state)
		{
			case P_STATE_NONE:
			{
				/* C-styled # line (#include, #define, etc), ignore rest of line */
				if (*line == '#')
				{
					return P_STATE_NONE;
				}
				/* single line comment, ignore rest of line */
				else if (*line == '/' && line[1] == '/')
				{
					return P_STATE_NONE;
				}
				/* multi-line comment */
				else if( *line == '/' && line[1] == '*' )
				{
					state = P_STATE_IN_COMMENT;
				}
				else if (isalnum((int) *line))
				{
					return parseResDefinition(line);
				}
				break;
			}
			case P_STATE_IN_COMMENT:
			{
				if (*line == '*' && line[1] == '/')
					state = P_STATE_NONE;
				break;
			}
			case P_STATE_IN_STATEMENTS:
			{
				/* First BEGIN block */
				if (*line == '{' || strcmp((const char *) line, "BEGIN") == 0)
				{
					_blockDepth = 1;
					return P_STATE_IN_BLOCK;
				}
				break;
			}
			case P_STATE_IN_BLOCK:
			{
				/* Nested BEGIN blocks? */
				if (*line == '{' || strcmp((const char *) line, "BEGIN") == 0)
				{
					_blockDepth++;
				}
				else if (*line == '}' || strcmp((const char *) line, "END") == 0)
				{
					if (_blockDepth == 1)
						return P_STATE_NONE;
					else
						_blockDepth--;
				}
				break;
			}
			case P_STATE_AT_END:
			{
				return state;
			}
		}
		if (line == NULL)
			return P_STATE_AT_END;
		line++;
	}

	return state;
}

static void findResTags(void)
{
	const unsigned char *line;
	ResParserState state = P_STATE_NONE;
	_blockDepth = 0;

	while ((line = readLineFromInputFile()) != NULL)
	{
		state = parseResLine(line, state);
		if (state == P_STATE_AT_END)
			return;
	}
}

/* parser definition */
extern parserDefinition* WindResParser(void)
{
	static const char *const extensions [] = { "rc", NULL };
	parserDefinition* def = parserNew("WindRes");
	def->kindTable	= ResKinds;
	def->kindCount	= ARRAY_SIZE(ResKinds);
	def->extensions	= extensions;
	def->parser		= findResTags;
	return def;
}
