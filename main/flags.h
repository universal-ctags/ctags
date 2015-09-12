/*
*
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Defines external interface to option processing.
*/
#ifndef _FLAGS_H
#define _FLAGS_H

#define LONG_FLAGS_OPEN  '{'
#define LONG_FLAGS_CLOSE '}'

typedef struct {
	char shortChar;
	const char *longStr;
	void (* shortProc) (char c,  void *data);
	void (* longProc)  (const char* const s, const char* const param, void *data);
	const char *paramName;
	const char *description;
} flagDefinition;

extern void flagsEval (const char* flags, flagDefinition* defs, unsigned int ndefs, void* data);
extern void flagPrintHelp (flagDefinition* def, unsigned int ndefs);

#endif	/* _FLAGS_H */
