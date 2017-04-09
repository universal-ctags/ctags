/*
*   Copyright (c) 2000-2003, Darren Hiebert
*   Copyright (c) 2017, Red Hat, Inc.
*   Copyright (c) 2017, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for applying regular expression matching.
*
*   The code for utilizing the Gnu regex package with regards to processing the
*   regex option and checking for regex matches was adapted from routines in
*   Gnu etags.
*/

#ifndef CTAGS_MAIN_LREGEX_H
#define CTAGS_MAIN_LREGEX_H
#include "general.h"

typedef struct {
	const char *const regex;
	const char* const name;
	const char* const kinds;
	const char *const flags;
	bool    *disabled;
} tagRegexTable;

typedef struct {
	size_t start;   /* character index in line where match starts */
	size_t length;  /* length of match */
} regexMatch;

typedef void (*regexCallback) (const char *line, const regexMatch *matches, unsigned int count,
			       void *userData);

struct lregexControlBlock;
extern struct lregexControlBlock* allocLregexControlBlock (parserDefinition *parser);
extern void freeLregexControlBlock (struct lregexControlBlock* lcb);

extern void processTagRegexOption (struct lregexControlBlock *lcb,
								   const char* const parameter);
extern void addTagRegex (struct lregexControlBlock *lcb, const char* const regex,
						 const char* const name, const char* const kinds, const char* const flags,
						 bool *disabled);
extern bool matchRegex (struct lregexControlBlock *lcb, const vString* const line);
extern bool hasScopeActionInRegex (struct lregexControlBlock *lcb);
extern void addCallbackRegex (struct lregexControlBlock *lcb,
							  const char* const regex,
							  const char* const flags,
							  const regexCallback callback,
							  bool *disabled,
							  void * userData);
extern bool hasMultilineRegexPatterns (struct lregexControlBlock *lcb);
extern bool matchMultilineRegex (struct lregexControlBlock *lcb, const vString* const allLines);

#endif	/* CTAGS_MAIN_LREGEX_H */
