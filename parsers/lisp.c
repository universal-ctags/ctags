/*
*   Copyright (c) 2000-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for LISP files.
*
*   References:
*
*   - [Lisp] https://www.lispworks.com/documentation/HyperSpec/Front/index.htm
*   - [EmacsLisp] https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "entry.h"
#include "field.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"
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
	K_TYPE,
	K_CLASS,
	K_STRUCT,
	K_METHOD,
	K_GENERIC,
	K_PARAMETER,
} lispKind;

static kindDefinition LispKinds [] = {
	{ true, 'Y', "unknown", "unknown type of definitions" },
	{ true, 'f', "function", "functions" },
	{ true, 'v', "variable", "variables" },
	{ true, 'm', "macro", "macros" },
	{ true, 'c', "const", "constants" },
	{ true, 't', "type", "types" },
	{ true, 'C', "class", "classes" },
	{ true, 's', "struct", "structs" },
	{ true, 'M', "method", "methods" },
	{ true, 'G', "generic", "generics" },
	{ true, 'p', "parameter", "parameters" },
};

typedef enum {
	F_DEFINER,
} lispField;

static fieldDefinition LispFields[] = {
	{ .name = "definer",
	  .description = "the name of the function or macro that defines the unknown/Y-kind object",
	  .enabled = true },
};

typedef enum {
	eK_UNKNOWN,
	eK_FUNCTION,
	eK_VARIABLE,
	eK_CONST,
	eK_MACRO,
	eK_ALIAS,
	eK_VARALIAS,
	eK_SUBST,
	eK_INLINE,
	eK_ERROR,
	eK_MINOR_MODE,
	eK_DERIVED_MODE,
	/* custom.el */
	eK_CUSTOM,
	eK_GROUP,
	eK_FACE,
	eK_THEME,
} emacsLispKind;

typedef enum {
	eF_DEFINER,
} emacsLispField;

static fieldDefinition EmacsLispFields[] = {
	{ .name = "definer",
	  .description = "the name of the function or macro that defines the unknown/Y-kind object",
	  .enabled = true },
};

/* Following macro/builtin doesn't define a name appeared
 * at car. So this parser doesn't handle it well.
 * -----------------------------------------------------
 * defadvice           (=> cadadr)
 * defconst-mode-local (=> cadr)
 * defvar-mode-local   (=> cadr)
 */
static kindDefinition EmacsLispKinds [] = {
	{ true, 'Y', "unknown", "unknown type of definitions" },
	{ true, 'f', "function", "functions" },
	{ true, 'v', "variable", "variables" },
	{ true, 'c', "const", "constants" },
	{ true, 'm', "macro", "macros" },
	{ true, 'a', "alias", "aliases for functions" },
	{ true, 'V', "varalias", "aliases for variables" },
	{ true, 's', "subst", "inline function" },
	{ true, 'i', "inline", "inline function" },
	{ true, 'e', "error", "errors" },
	{ true, 'M', "minorMode", "minor modes" },
	{ true, 'D', "derivedMode", "derived major mode" },
	/* custom.el */
	{ true, 'C', "custom", "customizable variables" },
	{ true, 'G', "group", "customization groups" },
	{ true, 'H', "face", "customizable faces" }, /* 'F' is reserved by ctags */
	{ true, 'T', "theme", "custom themes" },
};

struct lispDialect {
	int (* definer2kind) (const vString *const hint);
	bool case_insensitive;
	unsigned char namespace_sep;
	int unknown_kind;
	fieldDefinition *definer_field;
	bool skip_initial_spaces;
	bool (* is_def) (struct lispDialect *, const unsigned char *);
	int (* get_it) (struct lispDialect *,
					vString *const, const unsigned char *, vString *);
};

/*
*   FUNCTION DEFINITIONS
*/

/*
 * lisp tag functions
 *  look for (def or (DEF, quote or QUOTE
 */
static bool lisp_is_def (struct lispDialect *dialect, const unsigned char *strp)
{
	bool cis = dialect->case_insensitive; /* Renaming for making code short */
	bool is_def = ( (strp [1] == 'd' || (cis && strp [1] == 'D'))
					&& (strp [2] == 'e' || (cis && strp [2] == 'E'))
					&& (strp [3] == 'f' || (cis && strp [3] == 'F')));

	/* Ignore def"ault" */
	if (is_def
		&& (strp [4] == 'a' || (cis && strp [4] == 'A'))
		&& (strp [5] == 'u' || (cis && strp [5] == 'U'))
		&& (strp [6] == 'l' || (cis && strp [6] == 'L'))
		&& (strp [7] == 't' || (cis && strp [7] == 'T')))
		return false;

	/* Ignore def"inition" */
	if (is_def
		&& (strp  [4] == 'i' || (cis && strp  [4] == 'I'))
		&& (strp  [5] == 'n' || (cis && strp  [5] == 'N'))
		&& (strp  [6] == 'i' || (cis && strp  [6] == 'I'))
		&& (strp  [7] == 't' || (cis && strp  [7] == 'T'))
		&& (strp  [8] == 'i' || (cis && strp  [8] == 'I'))
		&& (strp  [9] == 'o' || (cis && strp  [9] == 'O'))
		&& (strp [10] == 'n' || (cis && strp [10] == 'N')))
		return false;

	return is_def;
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

static int L_issetf (const unsigned char *strp, bool case_insensitive)
{
	bool cis = case_insensitive; /* Renaming for making code short */

	return ( (*(++strp) == 's' || (cis && *strp == 'S'))
		  && (*(++strp) == 'e' || (cis && *strp == 'E'))
		  && (*(++strp) == 't' || (cis && *strp == 'T'))
		  && (*(++strp) == 'f' || (cis && *strp == 'F'))
		  && isspace (*(++strp)));
}

static int  lisp_hint2kind (const vString *const hint)
{
	int k = K_UNKNOWN;
	int n = vStringLength (hint) - 4;

	/* 4 means strlen("(def"). */
#define EQN(X) strncmp(vStringValue (hint) + 4, &X[3], n) == 0
	switch (n)
	{
	case 2:
		if (EQN("DEFUN"))
			k = K_FUNCTION;
		break;
	case 3:
		if (EQN("DEFVAR"))
			k = K_VARIABLE;
		break;
	case 4:
		if (EQN("DEFTYPE"))
			k = K_TYPE;
		break;
	case 5:
		if (EQN("DEFMACRO"))
			k = K_MACRO;
		else if (EQN("DEFCLASS"))
			k = K_CLASS;
		break;
	case 6:
		if (EQN("DEFSTRUCT"))
			k = K_STRUCT;
		else if (EQN("DEFMETHOD"))
			k = K_METHOD;
		break;
	case 7:
		if (EQN("DEFGENERIC"))
			k = K_GENERIC;
		break;
	case 8:
		if (EQN("DEFCONSTANT"))
			k = K_CONST;
		break;
	case 9:
		if (EQN("DEFPARAMETER"))
			k = K_PARAMETER;
		break;
	}
#undef EQN
	return k;
}

/* TODO: implement this in hashtable. */
static int  elisp_hint2kind (const vString *const hint)
{
	int k = eK_UNKNOWN;
	int n = vStringLength (hint) - 4;

	/* 4 means strlen("(def"). */
#define EQN(X) strncmp(vStringValue (hint) + 4, &X[3], n) == 0
	switch (n)
	{
	case 2:
		if (EQN("defun"))
			k = eK_FUNCTION;
		break;
	case 3:
		if (EQN("defvar"))
			k = eK_VARIABLE;
		else if (EQN("defun*"))
			k = eK_FUNCTION;
		break;
	case 4:
		if (EQN("defface"))
			k = eK_FACE;
	case 5:
		if (EQN("defconst"))
			k = eK_CONST;
		else if (EQN("defmacro"))
			k = eK_MACRO;
		else if (EQN("defalias"))
			k = eK_ALIAS;
		else if (EQN("defsubst"))
			k = eK_SUBST;
		else if (EQN("defgroup"))
			k = eK_GROUP;
		else if (EQN("deftheme"))
			k = eK_THEME;
		break;
	case 6:
		if (EQN("defcustom"))
			k = eK_CUSTOM;
		else if (EQN("defsubst*"))
			k = eK_SUBST;
		else if (EQN("defmacro*"))
			k = eK_MACRO;
		break;
	case 7:
		if (EQN("define-key"))
			k = KIND_GHOST_INDEX;
		break;
	case 9:
		if (EQN("defvar-local"))
			k = eK_VARIABLE;
		else if (EQN("define-error"))
			k = eK_ERROR;
		break;
	case 8:
		if (EQN("defvaralias"))
			k = eK_VARALIAS;
		break;
	case 10:
		if (EQN("define-inline"))
			k = eK_INLINE;
		break;
	case 14:
		if (EQN("define-minor-mode"))
			k = eK_MINOR_MODE;
		break;
	case 16:
		if (EQN("define-derived-mode"))
			k = eK_DERIVED_MODE;
		break;
	case 21:
		if (EQN("define-global-minor-mode"))
			k = eK_MINOR_MODE;
		break;
	case 25:
		if (EQN("define-globalized-minor-mode"))
			k = eK_MINOR_MODE;
		break;
	case 27:
		if (EQN("define-obsolete-function-alias"))
			k = eK_ALIAS;
		break;
	}
#undef EQN
	return k;
}


static int lisp_get_it (struct lispDialect *dialect,
						vString *const name, const unsigned char *dbp, vString *kind_hint)
{
	int index = CORK_NIL;
	const unsigned char *p;

	if (*dbp == '\'')  /* Skip prefix quote */
		dbp++;
	else if (*dbp == '(' && L_isquote (dbp, dialect->case_insensitive))  /* Skip "(quote " */
	{
		dbp += 7;
		while (isspace (*dbp))
			dbp++;
	}
	else if (*dbp == '(' && L_issetf (dbp, dialect->case_insensitive)) /* Skip "(setf " */
	{
		dbp += 6;
		while (isspace (*dbp))
			dbp++;
	}
	for (p=dbp ; *p!='\0' && *p!='(' && !isspace (*p) && *p!=')' ; p++)
		vStringPut (name, *p);

	if (vStringLength (name) > 0)
	{
		int kind =  dialect->definer2kind(kind_hint);
		const char *definer = NULL;

		if (kind == dialect->unknown_kind)
		{
			definer = vStringValue(kind_hint);
			if (definer[0] == '(')
				definer++;
		}

		if (kind != KIND_GHOST_INDEX)
		{
			index = makeSimpleTag (name, kind);
			if (dialect->definer_field && dialect->definer_field->enabled
				&& definer && index != CORK_NIL)
				attachParserFieldToCorkEntry (index, dialect->definer_field->ftype, definer);
		}
	}
	vStringClear (name);

	return index;
}

/* Algorithm adapted from from GNU etags.
 */
static void findLispTagsCommon (struct lispDialect *dialect)
{
	vString *name = vStringNew ();
	vString *kind_hint = vStringNew ();
	const unsigned char* p;


	while ((p = readLineFromInputFile ()) != NULL)
	{
		if (dialect->skip_initial_spaces)
		{
			while (isspace ((unsigned char) *p))
				p++;
		}

		if (*p == '(')
		{
			if (dialect->is_def (dialect, p))
			{
				vStringClear (kind_hint);
				while (*p != '\0' && !isspace (*p))
				{
					vStringPut (kind_hint,
								dialect->case_insensitive? toupper(*p): *p);
					p++;
				}
				while (isspace (*p))
					p++;
				dialect->get_it(dialect, name, p, kind_hint);
			}
			else if (dialect->namespace_sep != 0)
			{
				do
					p++;
				while (*p != '\0' && !isspace (*p)
						&& *p != dialect->namespace_sep && *p != '(' && *p != ')');
				if (*p == dialect->namespace_sep)
				{
					do
						p++;
					while (*p == dialect->namespace_sep);

					if (dialect->is_def (dialect, p - 1))
					{
						vStringClear (kind_hint);
						while (*p != '\0' && !isspace (*p))
						{
							vStringPut (kind_hint,
										dialect->case_insensitive? toupper(*p): *p);
							p++;
						}
						while (isspace (*p))
							p++;
						dialect->get_it(dialect, name, p, kind_hint);
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
	struct lispDialect lisp_dialect = {
		.definer2kind = lisp_hint2kind,
		.case_insensitive = true,
		.namespace_sep = ':',
		.unknown_kind = K_UNKNOWN,
		.definer_field = LispFields + F_DEFINER,
		.skip_initial_spaces = false,
		.is_def = lisp_is_def,
		.get_it = lisp_get_it,
	};

	findLispTagsCommon (&lisp_dialect);
}

static void findEmacsLispTags (void)
{
	struct lispDialect elisp_dialect = {
		.definer2kind = elisp_hint2kind,
		.case_insensitive = false,
		.namespace_sep = 0,
		.unknown_kind = eK_UNKNOWN,
		.definer_field = EmacsLispFields + eF_DEFINER,
		.skip_initial_spaces = false,
		.is_def = lisp_is_def,
		.get_it = lisp_get_it,
	};

	findLispTagsCommon (&elisp_dialect);
}

extern parserDefinition* LispParser (void)
{
	static const char *const extensions [] = {
		"cl", "clisp", "l", "lisp", "lsp", NULL
	};
	static const char *const aliases [] = {
		"clisp", NULL
	};

	static selectLanguage selectors[] = { selectLispOrLEXByLEXMarker, NULL };

	parserDefinition* def = parserNew ("Lisp");
	def->kindTable      = LispKinds;
	def->kindCount  = ARRAY_SIZE (LispKinds);
	def->fieldTable = LispFields;
	def->fieldCount = ARRAY_SIZE (LispFields);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser     = findLispTags;
	def->selectLanguage = selectors;
	def->useCork = CORK_QUEUE;
	def->versionCurrent = 0;
	def->versionAge = 1;
	return def;
}

extern parserDefinition* EmacsLispParser (void)
{
	static const char *const extensions [] = {
		"el", NULL
	};
	static const char *const aliases [] = {
		"emacs-lisp", NULL
	};

	parserDefinition* def = parserNew ("EmacsLisp");
	def->kindTable      = EmacsLispKinds;
	def->kindCount  = ARRAY_SIZE (EmacsLispKinds);
	def->fieldTable = EmacsLispFields;
	def->fieldCount = ARRAY_SIZE (EmacsLispFields);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser     = findEmacsLispTags;
	def->useCork = CORK_QUEUE;
	def->versionCurrent = 0;
	def->versionAge = 1;
	return def;
}
