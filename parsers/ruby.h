/*
*   Copyright (c) 2000-2001, Thaddeus Covert <sahuagin@mediaone.net>
*   Copyright (c) 2002 Matthias Veit <matthias_veit@yahoo.de>
*   Copyright (c) 2004 Elliott Hughes <enh@acm.org>
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Ruby language
*   files.
*/

#ifndef CTAGS_PARSER_RUBY_H
#define CTAGS_PARSER_RUBY_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "subparser.h"

typedef struct sRubySubparser rubySubparser;

struct sRubySubparser {
	subparser subparser;
	/* Returning other than CORK_NIL means the string is consumed. */
	int (* lineNotify) (rubySubparser *s, const unsigned char **cp);
	void (* enterBlockNotify) (rubySubparser *s, int corkIndex);
	void (* leaveBlockNotify) (rubySubparser *s, int corkIndex);
	/* Privately used in Ruby parser side. */
	int corkIndex;
};

extern void rubySkipWhitespace (const unsigned char **cp);
extern bool rubyCanMatchKeyword (const unsigned char** s, const char* literal);
extern bool rubyCanMatchKeywordWithAssign (const unsigned char** s, const char* literal);

extern bool rubyParseString (const unsigned char** cp, unsigned char boundary, vString* vstr);
extern bool rubyParseMethodName (const unsigned char **cp, vString* vstr);
extern bool rubyParseModuleName (const unsigned char **cp, vString* vstr);

#endif
