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

typedef struct {
	char short_char;
	char *long_str;
	void (* short_proc) (char c,  void *data);
	void (* long_proc)  (const char* const s, const char* const param, void *data);
  /* TODO: handler for help message */
} flagDefinition;

extern void flagsEval (const char* flags, flagDefinition* defs, unsigned int ndefs, void* data);

#endif	/* _FLAGS_H */
