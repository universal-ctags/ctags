/*
* Copyright (c) 2003, Peter Strand <peter@zarquon.se>
*
* This source code is released for free distribution under the terms of the
* GNU General Public License version 2 or (at your opinion) any later version.
*
* This module contains functions for generating tags for Haskell language
* files (https://en.wikipedia.org/wiki/Haskell_(programming_language)).
*
* Does not handle operators or infix definitions like:
* a `f` b = ...
*
*/


/*
*   INCLUDE FILES
*/

#include "general.h"    /* must always come first */

#include <string.h>

#include "parse.h"
#include "read.h"
#include "vstring.h"
#include "routines.h"


/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_TYPE, K_CONSTRUCTOR, K_FUNCTION, K_MODULE
} haskellKind;

static kindDefinition HaskellKinds [] = {
	{ true, 't', "type", "types" },
	{ true, 'c', "constructor", "type constructors" },
	{ true, 'f', "function", "functions" },
	{ true, 'm', "module", "modules"}
};


typedef const unsigned char *custr;

/*
*   FUNCTION DEFINITIONS
*/


static void skip_rest_of_line(void)
{
	int c;
	do {
		c = getcFromInputFile();
	} while (c != EOF && c != '\n');
}

static int get_line(char *buf)
{
	int i = 0;
	int c;
	do {
		c = getcFromInputFile();
		buf[i++] = c;
	} while (c != EOF && c != '\n' && i < 1000);
	buf[i] = '\0';
	return i;
}

static int get_next_char(void)
{
	int c, nxt;
	c = getcFromInputFile();
	if (c == EOF)
		return c;
	nxt = getcFromInputFile();
	if (nxt == EOF)
		return c;
	ungetcToInputFile(nxt);

	if (c == '-' && nxt == '-') {
		skip_rest_of_line();
		return get_next_char();
	}
	if (c == '{' && nxt == '-') {
		int last = '\0';
		do {
			last = c;
			c = get_next_char();
		} while (! (c == EOF || (last == '-' && c == '}')));
		return get_next_char();
	}
	return c;
}

static void add_tag(const char *token, haskellKind kind, vString *name)
{
	int i;
	for (i = 0; token[i] != '\0'; ++i)
		vStringPut(name, token[i]);

	makeSimpleTag(name, kind);
	vStringClear(name);
}

static int isident(char c)
{
	return isalnum(c) || c == '_' || c == '\'' || c == '$';
}

static int get_token(char *token, int n)
{
	int c = getcFromInputFile();
	int i = n;
	while (c != EOF && isident(c) && i < 1000) {
		token[i] = c;
		i++;
		c = getcFromInputFile();
	}
	token[i] = '\0';
	if (c == EOF)
		return 0;
	if (i != n) {
		ungetcToInputFile(c);
		return 1;
	} else {
		return 0;
	}
}

enum Find_State { Find_Eq, Find_Constr, Get_Extr, Find_Extr, Find_Bar };

static int inside_datatype(vString *name)
{
	enum Find_State st = Find_Eq;
	int c;
	char token[1001];

	while (1) {
		if (st == Find_Eq)
		{
			do {
				c = get_next_char();
				if (c == '\n') {
					c = get_next_char();
					if (! (c == ' ' || c == '\t')) {
						return c;
					}
				}
			} while (c != EOF && c != '=');
			st = Find_Constr;
		}
		else if (st == Find_Constr)
		{
			do {
				c = get_next_char();
			} while (isspace(c));
			if (!isupper(c)) {
				skip_rest_of_line();
				return '\n';
			}
			token[0] = c;
			if (!get_token(token, 1))
				return '\n';
			add_tag(token, K_CONSTRUCTOR, name);
			st = Find_Extr;
		}
		else if (st == Find_Extr)
		{
			c = get_next_char();
			if (c == '{')
				st = Get_Extr;
			else if (c == '|')
				st = Find_Constr;
			else if (c == '\n') {
				c = get_next_char();
				if (! (c == ' ' || c == '\t')) {
					return c;
				}
			}
			else if (!isspace(c))
				st = Find_Bar;
		}
		else if (st == Get_Extr)
		{
			do {
				c = get_next_char();
			} while (isspace(c));
			if (c == EOF)
				return c;
			token[0] = c;
			get_token(token, 1);
			add_tag(token, K_FUNCTION, name);
			do {
				c = get_next_char();
				if (c == '}') {
					st = Find_Bar;
					break;
				}
			} while (c != EOF && c != ',');
		}
		else if (st == Find_Bar)
		{
			do {
				c = get_next_char();
				if (c == '\n') {
					c = get_next_char();
					if (! (c == ' ' || c == '\t')) {
						return c;
					}
				}
			} while (c != EOF && c != '|');
			st = Find_Constr;
		}
	}
	return '\n';
}

static void findHaskellTags (int is_literate)
{
	vString *name = vStringNew ();
	char token[1001], arg[1001];
	int c;
	int in_tex_lit_code = 0;
	c = get_next_char();

	while (c != EOF)
	{
		if (c == '\n') {
			c = get_next_char();
			continue;
		}

		if (isspace(c)) {
			skip_rest_of_line();
			c = get_next_char();
			continue;
		}
		if (is_literate && !in_tex_lit_code) {
			if (c == '>') {
				c = getcFromInputFile();
				if (c == ' ') {
					c = get_next_char();
					if (!isident(c)) {
						skip_rest_of_line();
						c = get_next_char();
						continue;
					}
				} else {
					skip_rest_of_line();
					c = get_next_char();
					continue;
				}
			} else if (c == '\\') {
				int n = get_line(token);
				if (strncmp(token, "begin{code}", 11) == 0) {
					in_tex_lit_code = 1;
					c = get_next_char();
					continue;
				} else {
					if (n > 0 && token[n-1] != '\n')
						skip_rest_of_line();
					else
						c = get_next_char();
				}
				continue;
			} else {
				skip_rest_of_line();
				c = get_next_char();
				continue;
			}
		}
		if (is_literate && in_tex_lit_code && c == '\\') {
			get_line(token);
			if (strncmp(token, "end{code}", 9) == 0) {
				in_tex_lit_code = 0;
				c = get_next_char();
				continue;
			}
		}
		token[0] = c;
		if (!isident(c)) {
			skip_rest_of_line();
			c = get_next_char();
			continue;
		}
		if (!get_token(token, 1)) {
			c = get_next_char();
			continue;
		}
		do {
			if ((c = getcFromInputFile()) == EOF)
				return;
		} while (c == ' ' || c == '\t');
		arg[0] = c;
		get_token(arg, 1);
		if (strcmp(token, "data") == 0 || strcmp(token, "newtype") == 0) {
			add_tag(arg, K_TYPE, name);
			c = inside_datatype(name);
			continue;
		}
		if (strcmp(token, "type") == 0)
			add_tag(arg, K_TYPE, name);
		else if (strcmp(token, "module") == 0)
			add_tag(arg, K_MODULE, name);
		else if (strcmp(token, "instance") == 0 ||
				 strcmp(token, "foreign") == 0 ||
				 strcmp(token, "import") == 0)
			;
		else {
			if (arg[0] != ':')
				add_tag(token, K_FUNCTION, name);
		}
		skip_rest_of_line();
		c = get_next_char();
	}
	vStringDelete(name);
}

static void findNormalHaskellTags (void)
{
	findHaskellTags (0);
}

static void findLiterateHaskellTags (void)
{
	findHaskellTags (1);
}

extern parserDefinition* HaskellParser (void)
{
	static const char *const extensions [] = { "hs", NULL };
	parserDefinition* def  = parserNew ("Haskell");

	def->kindTable  = HaskellKinds;
	def->kindCount  = ARRAY_SIZE(HaskellKinds);
	def->extensions = extensions;
	def->parser     = findNormalHaskellTags;
	return def;
}

extern parserDefinition* LiterateHaskellParser (void)
{
	static const char *const extensions [] = { "lhs", NULL };
	parserDefinition* def = parserNew ("LiterateHaskell");
	def->kindTable  = HaskellKinds;
	def->kindCount  = ARRAY_SIZE(HaskellKinds);
	def->extensions = extensions;
	def->parser     = findLiterateHaskellTags;
	return def;
}
