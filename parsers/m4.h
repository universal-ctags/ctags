/*
*   Copyright (c) 2016, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for makefiles.
*/

#ifndef CTAGS_PARSER_M4_H
#define CTAGS_PARSER_M4_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "types.h"
#include "vstring.h"

struct m4HandleTokenResult
{
	int index;
	boolean consumed;
};

struct m4ParserClient
{
	langType lang;
	void *data;

	char quoteOpen;
	char quoteClose;

	/* Do something for new input file. Returned value is stored to
	   `data' field. */
	void*   (* prepareForNewInput) (void);
	boolean (* doesStartLineComment) (int c, const char *token, void *data);
	boolean (* doesStartStringLiteral) (int c, void *data);

	/* If the token is the part of the language the parser deals with,
	   return TRUE. This can be called before `prepareForNewInput' method. */
	boolean (* probeLanguage) (const char* token);

	struct m4HandleTokenResult (* handleMacro) (const char* token, void *data);

	struct m4ParserClient *host; /* Used internally */
};

/* The entry points for m4 meta parser */
extern void registerM4ParserClient (const char *hostLang, struct m4ParserClient *client);
extern void runM4Parser (langType lang);

/* Helpers functions */
extern boolean readM4MacroArgument(vString *const arg);

#endif
