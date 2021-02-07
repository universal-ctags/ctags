/*
*   Copyright (c) 2003-2004, Ascher Stefan <stievie@utanet.at>
*   Copyright (c) 2020, Masatake YAMATO
*   Copyright (c) 2020, Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*/

#ifndef CTAGS_PARSER_R_H
#define CTAGS_PARSER_R_H

/*
*   INCLUDE FILES
*/

#include "general.h"  /* must always come first */

#include "subparser.h"
#include "tokeninfo.h"
#include "entry.h"


/*
*   DATA DECLARATIONS
*/

typedef struct sRSubparser rSubparser;

enum RTokenType {
	/* 0..255 are the byte's values */
	TOKEN_R_EOF = 256,
	TOKEN_R_UNDEFINED,
	TOKEN_R_KEYWORD,
	TOKEN_R_NEWLINE,
	TOKEN_R_NUMBER,				/* 1, 1L */
	TOKEN_R_SYMBOL,				/* [0-9a-zA-Z._] */
	TOKEN_R_STRING,
	TOKEN_R_OPERATOR,				/* - + ! ~ ? : * / ^ %...%, <, > ==
								 * >=, <=, &, &&, |, || */
	TOKEN_R_DOTS,					/* ... */
	TOKEN_R_DOTS_N,				/* ..1, ..2, etc */
	TOKEN_R_LASSIGN,				/* <-, <<- */
	TOKEN_R_RASSIGN,				/* ->, ->> */
	TOKEN_R_SCOPE,				/* ::, ::: */
};

enum eRKeywordId
{
	KEYWORD_R_C,
	KEYWORD_R_DATAFRAME,
	KEYWORD_R_FUNCTION,
	KEYWORD_R_IF,
	KEYWORD_R_ELSE,
	KEYWORD_R_FOR,
	KEYWORD_R_WHILE,
	KEYWORD_R_REPEAT,
	KEYWORD_R_IN,
	KEYWORD_R_NEXT,
	KEYWORD_R_BREAK,
	KEYWORD_R_TRUE,
	KEYWORD_R_FALSE,
	KEYWORD_R_NULL,
	KEYWORD_R_INF,
	KEYWORD_R_LIST,
	KEYWORD_R_NAN,
	KEYWORD_R_NA,
	KEYWORD_R_SOURCE,
	KEYWORD_R_LIBRARY,
};

struct sRSubparser {
	subparser subparser;
	int (* readRightSideSymbol) (rSubparser *s,
								 tokenInfo *const symbol,
								 const char *const assignmentOperator,
								 int parent,
								 tokenInfo *const token);
	int (* makeTagWithTranslation) (rSubparser *s,
									tokenInfo *const token,
									int parent,
									bool in_func,
									int kindInR,
									const char *const assignmentOperator);
	bool (* askTagAcceptancy) (rSubparser *s, tagEntryInfo *pe);
	bool (* hasFunctionAlikeKind) (rSubparser *s, tagEntryInfo *pe);
	int (* readFuncall) (rSubparser *s,
						 tokenInfo *const func,
						 tokenInfo *const token,
						 int parent);
};

extern void rSetupCollectingSignature (tokenInfo *const token,
									   vString   *signature);
extern void rTeardownCollectingSignature (tokenInfo *const token);

/*
 * FUNCTION PROTOTYPES
 */

extern tokenInfo *rNewToken (void);

extern void rTokenReadNoNewline (tokenInfo *const token);

/* This function returns true if a new token is read.
 * EOF is exception. If EOF is read, this function returns FALSE. */
extern bool rParseStatement (tokenInfo *const token, int parentIndex, bool inArgList);

extern vString *rExtractNameFromString (vString* str);

#endif	/* CTAGS_PARSER_TEX_H */
