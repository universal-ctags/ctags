/*
*
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   Defines external interface to option processing.
*/
#ifndef CTAGS_MAIN_FLAGS_H
#define CTAGS_MAIN_FLAGS_H

#define LONG_FLAGS_OPEN  '{'
#define LONG_FLAGS_CLOSE '}'

typedef struct sFlagDefinition {
	char shortChar;
	const char *longStr;
	void (* shortProc) (char c,  void *data);
	void (* longProc)  (const char* const s, const char* const param, void *data);
	const char *paramName;
	const char *description;
} flagDefinition;

extern void flagsEval (const char* flags, flagDefinition* defs, unsigned int ndefs, void* data);
extern void flagPrintHelp (flagDefinition* def, unsigned int ndefs);

#endif	/* CTAGS_MAIN_FLAGS_H */
