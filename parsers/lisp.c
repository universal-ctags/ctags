/*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for LISP files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "parse.h"
#include "read.h"
#include "routines.h"
#include "vstring.h"

#include <string.h>

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_UNKNOWN,
	K_FUNCTION,
	K_VARIABLE,
	K_MACRO,
	K_CONST,
} lispKind;

static kindDefinition LispKinds [] = {
	{ true, 'u', "unknown", "unknown type of definitions" },
	{ true, 'f', "function", "functions" },
	{ true, 'v', "variable", "variables" },
	{ true, 'm', "macro", "macros" },
	{ true, 'c', "const", "constants" },
};

/*
*   FUNCTION DEFINITIONS
*/

/*
 * lisp tag functions
 *  look for (def or (DEF, quote or QUOTE
 */
static int L_isdef (const unsigned char *strp, bool case_insensitive)
{
	bool cis = case_insensitive; /* Renaming for making code short */

	return ( (strp [1] == 'd' || (cis && strp [1] == 'D'))
		  && (strp [2] == 'e' || (cis && strp [2] == 'E'))
		  && (strp [3] == 'f' || (cis && strp [3] == 'F')));
}

static int L_isquote (const unsigned char *strp, bool case_insensitive)
{
	bool cis = case_insensitive; /* Renaming for making code short */

	return ( (*(++strp) == 'q' || (cis && *strp == 'Q'))
		  && (*(++strp) == 'u' || (cis && *strp == 'U'))
		  && (*(++strp) == 'o' || (cis && *strp == 'O'))
		  && (*(++strp) == 't' || (cis && *strp == 'T'))
		  && (*(++strp) == 'e' || (cis && *strp == 'E'))
		  && isspace (*(++strp)));
}

static int  lisp_hint2kind (const vString *const hint)
{
	int k = K_UNKNOWN;
	int n;

	/* 4 means strlen("(def"). */
#define EQN(X) strncmp(vStringValue (hint) + 4, X + 3, n) == 0
	switch (vStringLength (hint) - 4)
	{
	case 2:
		n = 2;
		if (EQN("DEFUN"))
			k = K_FUNCTION;
		break;
	case 3:
		n = 3;
		if (EQN("DEFVAR"))
			k = K_VARIABLE;
		break;
	case 5:
		n = 5;
		if (EQN("DEFMACRO"))
			k = K_MACRO;
		break;
	case 8:
		n = 8;
		if (EQN("DEFCONSTANT"))
			k = K_CONST;
		break;
	}
#undef EQN
	return k;
}

static void L_getit (vString *const name, const unsigned char *dbp,
					 bool case_insensitive,
					 int (*hint2kind) (const vString *),
					 const vString *const kind_hint)
{
	const unsigned char *p;

	if (*dbp == '\'')  /* Skip prefix quote */
		dbp++;
	else if (*dbp == '(' && L_isquote (dbp, case_insensitive))  /* Skip "(quote " */
	{
		dbp += 7;
		while (isspace (*dbp))
			dbp++;
	}
	for (p=dbp ; *p!='\0' && *p!='(' && !isspace ((int) *p) && *p!=')' ; p++)
		vStringPut (name, *p);

	if (vStringLength (name) > 0)
	{
		int kind = hint2kind (kind_hint);
		if (kind != KIND_GHOST_INDEX)
			makeSimpleTag (name, kind);
	}
	vStringClear (name);
}

/* Algorithm adapted from from GNU etags.
 */
static void findLispTagsCommon (bool case_insensitive,
								bool has_namespace,
								int (*hint2kind) (const vString *))
{
	vString *name = vStringNew ();
	vString *kind_hint = vStringNew ();
	const unsigned char* p;


	while ((p = readLineFromInputFile ()) != NULL)
	{
		if (*p == '(')
		{
			if (L_isdef (p, case_insensitive))
			{
				vStringClear (kind_hint);
				while (*p != '\0' && !isspace ((int) *p))
				{
					vStringPut (kind_hint,
								case_insensitive? toupper((int)*p): *p);
					p++;
				}
				while (isspace ((int) *p))
					p++;
				L_getit (name, p, case_insensitive, hint2kind, kind_hint);
			}
			else if (has_namespace)
			{
				do
					p++;
				while (*p != '\0' && !isspace ((int) *p)
						&& *p != ':' && *p != '(' && *p != ')');
				if (*p == ':')
				{
					do
						p++;
					while (*p == ':');

					if (L_isdef (p - 1, case_insensitive))
					{
						vStringClear (kind_hint);
						while (*p != '\0' && !isspace ((int) *p))
						{
							vStringPut (kind_hint,
										case_insensitive? toupper((int)*p): *p);
							p++;
						}
						while (isspace (*p))
							p++;
						L_getit (name, p, case_insensitive, hint2kind, kind_hint);
					}
				}
			}
		}
	}
	vStringDelete (name);
	vStringDelete (kind_hint);
}

static void findLispTags (void)
{
	findLispTagsCommon (true, true, lisp_hint2kind);
}

extern parserDefinition* LispParser (void)
{
	static const char *const extensions [] = {
		"cl", "clisp", "el", "l", "lisp", "lsp", NULL
	};
	static const char *const aliases [] = {
		"clisp", "emacs-lisp", NULL
	};

	parserDefinition* def = parserNew ("Lisp");
	def->kindTable      = LispKinds;
	def->kindCount  = ARRAY_SIZE (LispKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser     = findLispTags;
	return def;
}
