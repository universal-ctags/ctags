/*
*   $Id$
*
*   Copyright (c) 1998-2001, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to get.c
*/
#ifndef _GET_H
#define _GET_H

/*
*   INCLUDE FILES
*/
#include "general.h"	/* must always come first */

#include "ctags.h"	/* to define langType */

/*
*   MACROS
*/
/*  Is the character valid as a character of a C identifier?
 */
#ifdef VMS
# define isident(c)	(isalnum(c) || (c) == '_' || (c) == '$')
#else
# define isident(c)	(isalnum(c) || (c) == '_')
#endif

/*  Is the character valid as the first character of a C identifier?
 */
#define isident1(c)	(isalpha(c) || (c) == '_' || (c) == '~')

/*
*   FUNCTION PROTOTYPES
*/
extern boolean isBraceFormat (void);
extern unsigned int getDirectiveNestLevel (void);
extern void cppInit (const boolean state);
extern void cppTerminate (void);
extern void cppBeginStatement (void);
extern void cppEndStatement (void);
extern void cppUngetc (const int c);
extern int cppGetc (void);

#endif	/* _GET_H */

/* vi:set tabstop=8 shiftwidth=4: */
