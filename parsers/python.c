/*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Python language
*   files.
*/
/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <stdio.h>

#include "entry.h"
#include "nestlevel.h"
#include "options.h"
#include "read.h"
#include "main.h"
#include "vstring.h"
#include "routines.h"
#include "debug.h"
#include "xtag.h"

/*
*   DATA DECLARATIONS
*/

typedef enum {
	K_UNDEFINED = -1,
	K_CLASS, K_FUNCTION, K_MEMBER, K_VARIABLE, K_NAMESPACE, K_MODULE, K_UNKNOWN,
} pythonKind;

typedef enum {
	PYTHON_MODULE_IMPORTED,
	PYTHON_MODULE_NAMESPACE,
	PYTHON_MODULE_INDIRECTLY_IMPORTED,
} pythonModuleRole;

typedef enum {
	PYTHON_UNKNOWN_IMPORTED,
	PYTHON_UNKNOWN_INDIRECTLY_IMPORTED,
} pythonUnknownRole;

/*
*   DATA DEFINITIONS
*/

/* Roles releated to `import'
 * ==========================
 * import X              X = (kind:module, role:imported)
 *
 * import X as Y         X = (kind:module, role:indirectly-imported),
 *                       Y = (kind:namespace, [nameref:X])
 *                       ------------------------------------------------
 *                       Don't confuse with namespace role of module kind.
 *
 * from X import *       X = (kind:module,  role:namespace)
 *
 * from X import Y       X = (kind:module,  role:namespace),
 *                       Y = (kind:unknown, role:imported, [scope:X])
 *
 * from X import Y as Z  X = (kind:module,  role:namespace),
 *                       Y = (kind:unknown, role:indirectly-imported, [scope:X])
 *                       Z = (kind:unknown, [nameref:X.Y]) */

static roleDesc PythonModuleRoles [] = {
	{ TRUE, "imported",
	  "imported modules" },
	{ TRUE, "namespace",
	  "namespace from where classes/variables/functions are imported" },
	{ TRUE, "indirectly-imported",
	  "module imported in alternative name" },
};

static roleDesc PythonUnknownRoles [] = {
	{ TRUE, "imported",   "imported from the other module" },
	{ TRUE, "indirectly-imported",
	  "classes/variables/functions/modules imported in alternative name" },
};

static kindOption PythonKinds[] = {
	{TRUE, 'c', "class",    "classes"},
	{TRUE, 'f', "function", "functions"},
	{TRUE, 'm', "member",   "class members"},
	{TRUE, 'v', "variable", "variables"},
	{TRUE, 'I', "namespace", "name referring a module defined in other file"},
	{TRUE, 'i', "module",    "modules",
	 .referenceOnly = TRUE,  ATTACH_ROLES(PythonModuleRoles)},
	{TRUE, 'x', "unknown",   "name referring a classe/variable/function/module defined in other module",
	 .referenceOnly = FALSE, ATTACH_ROLES(PythonUnknownRoles)},
};

typedef enum {
	A_PUBLIC, A_PRIVATE, A_PROTECTED
} pythonAccess;

static const char *const PythonAccesses[] = {
	"public", "private", "protected"
};

static char const * const singletriple = "'''";
static char const * const doubletriple = "\"\"\"";

/*
*   FUNCTION DEFINITIONS
*/

static boolean isIdentifierFirstCharacter (int c)
{
	return (boolean) (isalpha (c) || c == '_');
}

static boolean isIdentifierFirstCharacterCB (int c, void *dummy __unused__)
{
	return isIdentifierFirstCharacter (c);
}

static boolean isModuleFirstCharacterCB (int c, void *dummy __unused__)
{
	return (boolean) (isIdentifierFirstCharacter (c) || c == '.');
}

static boolean isIdentifierCharacter (int c)
{
	return (boolean) (isalnum (c) || c == '_');
}

static boolean isModuleCharacter (int c)
{
	return (boolean) (isIdentifierCharacter (c) || c == '.');
}

/* follows PEP-8, and always reports single-underscores as protected
 * See:
 * - http://www.python.org/dev/peps/pep-0008/#method-names-and-instance-variables
 * - http://www.python.org/dev/peps/pep-0008/#designing-for-inheritance
 */
static pythonAccess accessFromIdentifier (const vString *const ident,
	pythonKind kind, boolean has_parent, boolean parent_is_class)
{
	const char *const p = vStringValue (ident);
	const size_t len = vStringLength (ident);

	/* inside a function/method, private */
	if (has_parent && !parent_is_class)
		return A_PRIVATE;
	/* not starting with "_", public */
	else if (len < 1 || p[0] != '_')
		return A_PUBLIC;
	/* "__...__": magic methods */
	else if (kind == K_MEMBER && parent_is_class &&
			 len > 3 && p[1] == '_' && p[len - 2] == '_' && p[len - 1] == '_')
		return A_PUBLIC;
	/* "__...": name mangling */
	else if (parent_is_class && len > 1 && p[1] == '_')
		return A_PRIVATE;
	/* "_...": suggested as non-public, but easily accessible */
	else
		return A_PROTECTED;
}

static void addAccessFields (tagEntryInfo *const entry,
	const vString *const ident, pythonKind kind,
	boolean has_parent, boolean parent_is_class)
{
	pythonAccess access;

	access = accessFromIdentifier (ident, kind, has_parent, parent_is_class);
	entry->extensionFields.access = PythonAccesses [access];
	/* FIXME: should we really set isFileScope in addition to access? */
	if (access == A_PRIVATE)
		entry->isFileScope = TRUE;
}

/* Given a string with the contents of a line directly after the "def" keyword,
 * extract all relevant information and create a tag.
 */
static void makeFunctionTagFull (tagEntryInfo *tag, vString *const function,
				 vString *const parent, int is_class_parent, const char *arglist)
{
	char scope_kind_letter = KIND_NULL;

	if (is_class_parent)
	{
		if (!PythonKinds[K_MEMBER].enabled)
			return;
	}
	else
	{
		if (!PythonKinds[K_FUNCTION].enabled)
			return;
	}

	tag->extensionFields.signature = arglist;

	if (vStringLength (parent) > 0)
	{
		if (is_class_parent)
		{
			tag->kind = &(PythonKinds[K_MEMBER]);
			tag->extensionFields.scopeKind = &(PythonKinds[K_CLASS]);
			tag->extensionFields.scopeName = vStringValue (parent);
			scope_kind_letter = PythonKinds[K_CLASS].letter;
		}
		else
		{
			tag->extensionFields.scopeKind = &(PythonKinds[K_FUNCTION]);
			tag->extensionFields.scopeName = vStringValue (parent);
		}
	}

	addAccessFields (tag, function, is_class_parent ? K_MEMBER : K_FUNCTION,
		vStringLength (parent) > 0, is_class_parent);

	makeTagEntry (tag);

	if ((scope_kind_letter != KIND_NULL)
	    && tag->extensionFields.scopeName)
		makeQualifiedTagEntry (tag);
}

static void makeFunctionTag (vString *const function,
	vString *const parent, int is_class_parent, const char *arglist)
{
	tagEntryInfo tag;

	initTagEntry (&tag, vStringValue (function), &(PythonKinds[K_FUNCTION]));
	makeFunctionTagFull (&tag, function, parent, is_class_parent, arglist);
}

/* Given a string with the contents of the line directly after the "class"
 * keyword, extract all necessary information and create a tag.
 */
static void makeClassTag (vString *const class, vString *const inheritance,
	vString *const parent, int is_class_parent)
{
	tagEntryInfo tag;

	if (! PythonKinds[K_CLASS].enabled)
		return;

	initTagEntry (&tag, vStringValue (class), &(PythonKinds[K_CLASS]));
	if (vStringLength (parent) > 0)
	{
		if (is_class_parent)
		{
			tag.extensionFields.scopeKind = &(PythonKinds[K_CLASS]);
			tag.extensionFields.scopeName = vStringValue (parent);
		}
		else
		{
			tag.extensionFields.scopeKind = &(PythonKinds[K_FUNCTION]);
			tag.extensionFields.scopeName = vStringValue (parent);
		}
	}
	tag.extensionFields.inheritance = vStringValue (inheritance);
	addAccessFields (&tag, class, K_CLASS, vStringLength (parent) > 0,
		is_class_parent);
	makeTagEntry (&tag);
}

static void makeVariableTag (vString *const var, vString *const parent,
	boolean is_class_parent)
{
	tagEntryInfo tag;

	if (! PythonKinds[K_VARIABLE].enabled)
		return;

	initTagEntry (&tag, vStringValue (var), &(PythonKinds[K_VARIABLE]));
	if (vStringLength (parent) > 0)
	{
		tag.extensionFields.scopeKind = &(PythonKinds[K_CLASS]);
		tag.extensionFields.scopeName = vStringValue (parent);
	}
	addAccessFields (&tag, var, K_VARIABLE, vStringLength (parent) > 0,
		is_class_parent);
	makeTagEntry (&tag);
}

/* Skip a single or double quoted string. */
static const char *skipString (const char *cp)
{
	const char *start = cp;
	int escaped = 0;
	for (cp++; *cp; cp++)
	{
		if (escaped)
			escaped--;
		else if (*cp == '\\')
			escaped++;
		else if (*cp == *start)
			return cp + 1;
	}
	return cp;
}

static const char *skipUntil (const char *cp,
			      boolean (* isAcceptable) (int, void*),
			      void *user_data)
{
	int match;
	for (; *cp; cp++)
	{
		if (*cp == '#')
			return strchr(cp, '\0');

		match = 0;
		if (*cp == '"' || *cp == '\'')
			match = 1;

		/* these checks find unicode, binary (Python 3) and raw strings */
		if (!match)
		{
			boolean r_first = (*cp == 'r' || *cp == 'R');

			/* "r" | "R" | "u" | "U" | "b" | "B" */
			if (r_first || *cp == 'u' || *cp == 'U' ||  *cp == 'b' || *cp == 'B')
			{
				unsigned int i = 1;

				/*  r_first -> "rb" | "rB" | "Rb" | "RB"
				   !r_first -> "ur" | "UR" | "Ur" | "uR" | "br" | "Br" | "bR" | "BR" */
				if (( r_first && (cp[i] == 'b' || cp[i] == 'B')) ||
					(!r_first && (cp[i] == 'r' || cp[i] == 'R')))
					i++;

				if (cp[i] == '\'' || cp[i] == '"')
				{
					match = 1;
					cp += i;
				}
			}
		}
		if (match)
		{
			cp = skipString(cp);
			if (!*cp) break;
		}
		if (isAcceptable ((int) *cp, user_data))
			return cp;
		if (match)
			cp--; /* avoid jumping over the character after a skipped string */
	}
	return cp;
}

/* Skip everything up to an identifier start. */
static const char *skipToNextIdentifier (const char *cp)
{
	return skipUntil (cp, isIdentifierFirstCharacterCB, NULL);
}

/* Skip everything up to a module start. */
static const char *skipToNextModule (const char *cp)
{
	return skipUntil (cp, isModuleFirstCharacterCB, NULL);
}


/* Skip an identifier. */
static const char *skipIdentifier (const char *cp)
{
	while (isIdentifierCharacter ((int) *cp))
		cp++;
	return cp;
}

static const char *findDefinitionOrClass (const char *cp)
{
	while (*cp)
	{
		cp = skipToNextIdentifier (cp);
		if (!strncmp(cp, "def", 3) || !strncmp(cp, "class", 5) ||
			!strncmp(cp, "cdef", 4) || !strncmp(cp, "cpdef", 5))
		{
			return cp;
		}
		cp = skipIdentifier (cp);
	}
	return NULL;
}

static const char *skipSpace (const char *cp)
{
	while (isspace ((int) *cp))
		++cp;
	return cp;
}

/* Starting at ''cp'', parse an identifier into ''identifier''. */
static const char *parseIdentifier (const char *cp, vString *const identifier)
{
	vStringClear (identifier);
	while (isIdentifierCharacter ((int) *cp))
	{
		vStringPut (identifier, (int) *cp);
		++cp;
	}
	vStringTerminate (identifier);
	return cp;
}

static const char *parseModule (const char *cp, vString *const module)
{
	vStringClear (module);
	while (isModuleCharacter (*cp))
	{
		vStringPut (module, (int) *cp);
		++cp;
	}
	vStringTerminate (module);
	return cp;
}

static void parseClass (const char *cp, vString *const class,
	vString *const parent, int is_class_parent)
{
	vString *const inheritance = vStringNew ();
	vStringClear (inheritance);
	cp = parseIdentifier (cp, class);
	cp = skipSpace (cp);
	if (*cp == '(')
	{
		++cp;
		while (*cp != ')')
		{
			if (*cp == '\0')
			{
				/* Closing parenthesis can be in follow up line. */
				cp = (const char *) readLineFromInputFile ();
				if (!cp) break;
				vStringPut (inheritance, ' ');
				continue;
			}
			vStringPut (inheritance, *cp);
			++cp;
		}
		vStringTerminate (inheritance);
	}
	makeClassTag (class, inheritance, parent, is_class_parent);
	vStringDelete (inheritance);
}

static void parseImports (const char *cp, const char* from_module)
{
	const char* cp_next;
	vString *name, *name_next, *fq;
	boolean maybe_multiline = FALSE;
	boolean found_multiline_end = FALSE;

	name = vStringNew ();
	name_next = vStringNew ();
	fq = vStringNew ();

	cp = skipSpace (cp);
	if (from_module && *cp == '(')
	{
		maybe_multiline = TRUE;
		++cp;
	}

	cp = skipToNextModule (cp);
nextLine:
	while (*cp)
	{
		cp = parseModule (cp, name);
		cp = skipSpace (cp);
		if (*cp == ')')
			found_multiline_end = TRUE;
		cp = skipToNextModule (cp);
		cp_next = parseIdentifier (cp, name_next);

		if (strcmp (vStringValue (name_next), "as") == 0)
		{
			cp = skipToNextIdentifier (cp_next);
			cp = parseIdentifier (cp, name_next);
			if (from_module)
			{
				/* from x import Y as Z
				   ----------------------------
				   x = (kind:module,  role:namespace),
				   Y = (kind:unknown, role:indirectly-imported),
				   Z = (kind:unknown) */

				/* Y */
				makeSimpleRefTag (name, PythonKinds, K_UNKNOWN,
						  PYTHON_UNKNOWN_INDIRECTLY_IMPORTED);
				/* x.Y */
				if (isXtagEnabled(XTAG_QUALIFIED_TAGS))
				{
					tagEntryInfo fqe;
					vStringCatS (fq, from_module);
					vStringPut (fq, '.');
					vStringCat (fq, name);
					initRefTagEntry (&fqe, vStringValue (fq), PythonKinds + K_UNKNOWN,
							 PYTHON_UNKNOWN_INDIRECTLY_IMPORTED);
					markTagExtraBit (&fqe, XTAG_QUALIFIED_TAGS);
					makeTagEntry (&fqe);
					vStringClear(fq);
				}
				/* Z */
				makeSimpleTag (name_next, PythonKinds, K_UNKNOWN);
			}
			else
			{
				/* import x as Y
				   ----------------------------
				   X = (kind:module, role:indirectly-imported)
				   Y = (kind:namespace)*/
				/* X */
				makeSimpleRefTag (name, PythonKinds, K_MODULE,
						  PYTHON_MODULE_INDIRECTLY_IMPORTED);
				/* Y */
				makeSimpleTag (name_next, PythonKinds, K_NAMESPACE);
			}

			cp = skipSpace (cp);
			if (*cp == ')')
			{
				found_multiline_end = TRUE;
				cp++;
			}
			cp = skipToNextIdentifier (cp);
		}
		else
		{
			if (from_module)
			{
				/* from x import Y
				   --------------
				   x = (kind:module,  role:namespace),
				   Y = (kind:unknown, role:imported) */
				/* Y */
				makeSimpleRefTag (name, PythonKinds, K_UNKNOWN,
						  PYTHON_MODULE_IMPORTED);
				/* x.Y */
				if (isXtagEnabled(XTAG_QUALIFIED_TAGS))
				{
					tagEntryInfo fqe;
					vStringCatS (fq, from_module);
					vStringPut (fq, '.');
					vStringCat (fq, name);
					initRefTagEntry (&fqe, vStringValue (fq),
							 PythonKinds + K_UNKNOWN,
							 PYTHON_MODULE_IMPORTED);
					markTagExtraBit (&fqe, XTAG_QUALIFIED_TAGS);
					makeTagEntry (&fqe);
					vStringClear(fq);
				}
			}
			else
			{
				/* import X
				   --------------
				   X = (kind:module, role:imported) */
				makeSimpleRefTag (name, PythonKinds, K_MODULE,
						  PYTHON_MODULE_IMPORTED);
			}
			/* Don't update cp. Start from the position of name_next. */
		}
	}

	if (maybe_multiline && (!found_multiline_end))
	{
		if ((cp = (const char *) readLineFromInputFile ()) != NULL)
		{
			cp = skipSpace (cp);
			if (*cp == ')')
			{
				cp++;
				found_multiline_end = TRUE;
			}
			else
				goto nextLine;
		}
	}

	vStringDelete (fq);
	vStringDelete (name);
	vStringDelete (name_next);
}

static void parseFromModule (const char *cp, const char* dummy __unused__)
{
	vString *from_module;
	vString *import_keyword;

	/* from X import ...
	   --------------------
	   X = (kind:module, role:namespace) */

	from_module = vStringNew ();
	import_keyword = vStringNew ();

	cp = skipToNextModule (cp);
	cp = parseModule (cp, from_module);
	cp = skipToNextIdentifier (cp);
	cp = parseIdentifier (cp, import_keyword);

	if (strcmp (vStringValue (import_keyword), "import") == 0
	    || strcmp (vStringValue (import_keyword), "cimport") == 0)
	{
		makeSimpleRefTag (from_module, PythonKinds, K_MODULE,
				  PYTHON_MODULE_NAMESPACE);
		parseImports (cp, vStringValue (from_module));
	}

	vStringDelete (import_keyword);
	vStringDelete (from_module);
}


static boolean parseNamespace (const char *cp)
{
	void (* parse_sub) (const char *, const char *);

	cp = skipToNextIdentifier (cp);

	if (strncmp (cp, "import", 6) == 0)
	{
		cp += 6;
		parse_sub = parseImports;
	}
	else if (strncmp (cp, "cimport", 7) == 0)
	{
		cp += 7;
		parse_sub = parseImports;
	}
	else if (strncmp (cp, "from", 4) == 0)
	{
		cp += 4;
		parse_sub = parseFromModule;
	}
	else
		return FALSE;

	/* continue only if there is some space between the keyword and the identifier */
	if (! isspace (*cp))
		return FALSE;

	cp++;
	cp = skipSpace (cp);

	parse_sub (cp, NULL);
	return TRUE;
}

/* modified from get.c getArglistFromStr().
 * warning: terminates rest of string past arglist!
 * note: does not ignore brackets inside strings! */
struct argParsingState
{
	vString *arglist;
	int level;
};

static boolean gatherArglistCB (int c, void *arglist)
{
	if (arglist)
	{
		if ('\t' == c)
			c = ' ';

		if (vStringLast ((vString *)arglist) != ' '
		    || c != ' ')
			vStringPut ((vString *)arglist, c);
	}

	if (c == '(' || c == ')')
		return TRUE;
	else
		return FALSE;
}

static boolean parseArglist(const char* buf, struct argParsingState *state)
{
	const char *start, *current;

	start = buf;
	if (state->level == 0)
	{
		if (NULL == (start = strchr(buf, '(')))
			return FALSE;
		else
		{
			if (state->arglist)
				vStringPut (state->arglist, *start);
			state->level = 1;
			start += 1;
		}
	}


	do {
		current = skipUntil (start, gatherArglistCB, state->arglist);
		switch (*current)
		{
		case '\0':
			break;
		case '(':
			++ state->level;
			break;
		case ')':
			-- state->level;
			break;
		}
		start = current + 1;
	} while (
		/* Still be in parenthesis */
		state->level > 0
		/* the input string is continued. */
		&& *current && *start
		);

	return TRUE;
}

static void captureArguments (const char *start, vString *arglist)
{
	struct argParsingState state;

	state.level = 0;
	state.arglist = arglist;

	while (start)
	{
		if (parseArglist (start, &state) == FALSE)
			/* No '(' is found: broken input */
			break;
		else if (state.level == 0)
			break;
		else
			start = (const char *) readLineFromInputFile ();
	}
}

static void skipParens (const char *start)
{
	captureArguments (start, NULL);
}

static void parseFunction (const char *cp, vString *const def,
	vString *const parent, int is_class_parent)
{
	tagEntryInfo tag;
	static vString *arglist;

	cp = parseIdentifier (cp, def);
	initTagEntry (&tag, vStringValue (def), &(PythonKinds[K_FUNCTION]));

	if (arglist)
	  vStringClear (arglist);
	else
	  arglist = vStringNew ();
	captureArguments (cp, arglist);
	makeFunctionTagFull (&tag, def, parent, is_class_parent, vStringValue (arglist));
}

/* Get the combined name of a nested symbol. Classes are separated with ".",
 * functions with "/". For example this code:
 * class MyClass:
 *     def myFunction:
 *         def SubFunction:
 *             class SubClass:
 *                 def Method:
 *                     pass
 * Would produce this string:
 * MyClass.MyFunction/SubFunction/SubClass.Method
 */
static boolean constructParentString(NestingLevels *nls, int indent,
	vString *result)
{
	int i;
	NestingLevel *prev = NULL;
	int is_class = FALSE;
	vStringClear (result);
	for (i = 0; i < nls->n; i++)
	{
		NestingLevel *nl = nls->levels + i;
		if (indent <= nl->indentation)
			break;
		if (prev)
		{
			vStringCatS(result, ".");	/* make Geany symbol list grouping work properly */
/*
			if (prev->kindIndex == K_CLASS)
				vStringCatS(result, ".");
			else
				vStringCatS(result, "/");
*/
		}
		vStringCat(result, nl->name);
		is_class = (nl->kindIndex == K_CLASS);
		prev = nl;
	}
	return is_class;
}

/* Check indentation level and truncate nesting levels accordingly */
static void checkIndent(NestingLevels *nls, int indent)
{
	int i;
	NestingLevel *n;

	for (i = 0; i < nls->n; i++)
	{
		n = nls->levels + i;
		if (n && indent <= n->indentation)
		{
			/* truncate levels */
			nls->n = i;
			break;
		}
	}
}

static void addNestingLevel(NestingLevels *nls, int indentation,
	const vString *name, boolean is_class)
{
	int i;
	NestingLevel *nl = NULL;
	int kindIndex = is_class ? K_CLASS : K_FUNCTION;

	for (i = 0; i < nls->n; i++)
	{
		nl = nls->levels + i;
		if (indentation <= nl->indentation) break;
	}
	if (i == nls->n)
		nl = nestingLevelsPush(nls, name, kindIndex);
	else
		/* reuse existing slot */
		nl = nestingLevelsTruncate (nls, i + 1, name, kindIndex);

	nl->indentation = indentation;
}

/* Return a pointer to the start of the next triple string, or NULL. Store
 * the kind of triple string in "which" if the return is not NULL.
 */
static char const *find_triple_start(char const *string, char const **which)
{
	char const *cp = string;

	for (; *cp; cp++)
	{
		if (*cp == '#')
			break;
		if (*cp == '"' || *cp == '\'')
		{
			if (strncmp(cp, doubletriple, 3) == 0)
			{
				*which = doubletriple;
				return cp;
			}
			if (strncmp(cp, singletriple, 3) == 0)
			{
				*which = singletriple;
				return cp;
			}
			cp = skipString(cp);
			if (!*cp) break;
			cp--; /* avoid jumping over the character after a skipped string */
		}
	}
	return NULL;
}

/* Find the end of a triple string as pointed to by "which", and update "which"
 * with any other triple strings following in the given string.
 */
static void find_triple_end(char const *string, char const **which)
{
	char const *s = string;
	while (1)
	{
		/* Check if the string ends in the same line. */
		s = strstr (s, *which);
		if (!s) break;
		s += 3;
		*which = NULL;
		/* If yes, check if another one starts in the same line. */
		s = find_triple_start(s, which);
		if (!s) break;
		s += 3;
	}
}

static const char *findVariable(const char *line, const char** lineContinuation)
{
	/* Parse global and class variable names (C.x) from assignment statements.
	 * Object attributes (obj.x) are ignored.
	 * Assignment to a tuple 'x, y = 2, 3' not supported.
	 * TODO: ignore duplicate tags from reassignment statements. */
	const char *cp, *sp, *eq, *start;

	cp = strstr(line, "=");
	if (!cp)
		return NULL;
	eq = cp + 1;
	while (*eq)
	{
		if (*eq == '=')
			return NULL;	/* ignore '==' operator and 'x=5,y=6)' function lines */
		if (*eq == '(' || *eq == '#')
			break;	/* allow 'x = func(b=2,y=2,' lines and comments at the end of line */
		eq++;
	}

	if (*eq == '(')
		*lineContinuation = eq;

	/* go backwards to the start of the line, checking we have valid chars */
	start = cp - 1;
	while (start >= line && isspace ((int) *start))
		--start;
	while (start >= line && isIdentifierCharacter ((int) *start))
		--start;
	if (!isIdentifierFirstCharacter(*(start + 1)))
		return NULL;
	sp = start;
	while (sp >= line && isspace ((int) *sp))
		--sp;
	if ((sp + 1) != line)	/* the line isn't a simple variable assignment */
		return NULL;
	/* the line is valid, parse the variable name */
	++start;
	return start;
}

/* Skip type declaration that optionally follows a cdef/cpdef */
static const char *skipTypeDecl (const char *cp, boolean *is_class)
{
	const char *lastStart = cp, *ptr = cp;
	int loopCount = 0;
	ptr = skipSpace(cp);
	if (!strncmp("extern", ptr, 6)) {
		ptr += 6;
		ptr = skipSpace(ptr);
		if (!strncmp("from", ptr, 4)) { return NULL; }
	}
	if (!strncmp("class", ptr, 5)) {
		ptr += 5 ;
		*is_class = TRUE;
		ptr = skipSpace(ptr);
		return ptr;
	}
	/* limit so that we don't pick off "int item=obj()" */
	while (*ptr && loopCount++ < 2) {
		while (*ptr && *ptr != '=' && *ptr != '(' && !isspace(*ptr)) {
			/* skip over e.g. 'cpdef numpy.ndarray[dtype=double, ndim=1]' */
			if(*ptr == '[') {
				while (*ptr && *ptr != ']') ptr++;
				if (*ptr) ptr++;
			} else {
				ptr++;
			}
		}
		if (!*ptr || *ptr == '=') return NULL;
		if (*ptr == '(') {
			return lastStart; /* if we stopped on a '(' we are done */
		}
		ptr = skipSpace(ptr);
		lastStart = ptr;
		while (*lastStart == '*') lastStart++;  /* cdef int *identifier */
	}
	return NULL;
}

/* checks if there is a lambda at position of cp, and return its argument list
 * if so.
 * We don't return the lambda name since it is useless for now since we already
 * know it when we call this function, and it would be a little slower. */
static boolean varIsLambda (const char *cp, char **arglist)
{
	boolean is_lambda = FALSE;

	cp = skipSpace (cp);
	cp = skipIdentifier (cp); /* skip the lambda's name */
	cp = skipSpace (cp);
	if (*cp == '=')
	{
		cp++;
		cp = skipSpace (cp);
		if (strncmp (cp, "lambda", 6) == 0)
		{
			const char *tmp;

			cp += 6; /* skip the lambda */
			tmp = skipSpace (cp);
			/* check if there is a space after lambda to detect assignations
			 * starting with 'lambdaXXX' */
			if (tmp != cp)
			{
				vString *args = vStringNew ();

				cp = tmp;
				vStringPut (args, '(');
				for (; *cp != 0 && *cp != ':'; cp++)
					vStringPut (args, *cp);
				vStringPut (args, ')');
				vStringTerminate (args);
				if (arglist)
					*arglist = strdup (vStringValue (args));
				vStringDelete (args);
				is_lambda = TRUE;
			}
		}
	}
	return is_lambda;
}

/* checks if @p cp has keyword @p keyword at the start, and fills @p cp_n with
 * the position of the next non-whitespace after the keyword */
static boolean matchKeyword (const char *keyword, const char *cp, const char **cp_n)
{
	size_t kw_len = strlen (keyword);
	if (strncmp (cp, keyword, kw_len) == 0 && isspace (cp[kw_len]))
	{
		*cp_n = skipSpace (&cp[kw_len + 1]);
		return TRUE;
	}
	return FALSE;
}

static void findPythonTags (void)
{
	vString *const continuation = vStringNew ();
	vString *const name = vStringNew ();
	vString *const parent = vStringNew();

	NestingLevels *const nesting_levels = nestingLevelsNew();

	const char *line;
	int line_skip = 0;
	char const *longStringLiteral = NULL;

	while ((line = (const char *) readLineFromInputFile ()) != NULL)
	{
		const char *variableLineContinuation = NULL;
		const char *cp = line, *candidate;
		char const *longstring;
		char const *keyword, *variable;
		int indent;

		cp = skipSpace (cp);

		if (*cp == '\0')  /* skip blank line */
			continue;

		/* Skip comment if we are not inside a multi-line string. */
		if (*cp == '#' && !longStringLiteral)
			continue;

		/* Deal with line continuation. */
		if (!line_skip) vStringClear(continuation);
		vStringCatS(continuation, line);
		vStringStripTrailing(continuation);
		if (vStringLast(continuation) == '\\')
		{
			vStringChop(continuation);
			vStringCatS(continuation, " ");
			line_skip = 1;
			continue;
		}
		cp = line = vStringValue(continuation);
		cp = skipSpace (cp);
		indent = cp - line;
		line_skip = 0;

		/* Deal with multiline string ending. */
		if (longStringLiteral)
		{
			find_triple_end(cp, &longStringLiteral);
			continue;
		}

		checkIndent(nesting_levels, indent);

		/* Find global and class variables */
		variable = findVariable(line, &variableLineContinuation);
		if (variable)
		{
			const char *start = variable;
			char *arglist;
			boolean parent_is_class;

			vStringClear (name);
			while (isIdentifierCharacter ((int) *start))
			{
				vStringPut (name, (int) *start);
				++start;
			}
			vStringTerminate (name);

			parent_is_class = constructParentString(nesting_levels, indent, parent);
			if (varIsLambda (variable, &arglist))
			{
				/* show class members or top-level script lambdas only */
				if (parent_is_class || vStringLength(parent) == 0)
					makeFunctionTag (name, parent, parent_is_class, arglist);
				eFree (arglist);
			}
			else
			{
				/* skip variables in methods */
				if (parent_is_class || vStringLength(parent) == 0)
					makeVariableTag (name, parent, parent_is_class);
			}

			if (variableLineContinuation)
			{
				skipParens (variableLineContinuation);
				continue;
			}
		}

		/* Deal with multiline string start. */
		longstring = find_triple_start(cp, &longStringLiteral);
		if (longstring)
		{
			longstring += 3;
			find_triple_end(longstring, &longStringLiteral);
			/* We don't parse for any tags in the rest of the line. */
			continue;
		}

		/* Deal with def and class keywords. */
		keyword = findDefinitionOrClass (cp);
		if (keyword)
		{
			boolean found = FALSE;
			boolean is_class = FALSE;
			if (matchKeyword ("def", keyword, &cp))
			{
				found = TRUE;
			}
			else if (matchKeyword ("class", keyword, &cp))
			{
				found = TRUE;
				is_class = TRUE;
			}
			else if (matchKeyword ("cdef", keyword, &cp))
			{
				candidate = skipTypeDecl (cp, &is_class);
				if (candidate)
				{
					found = TRUE;
					cp = candidate;
				}

			}
			else if (matchKeyword ("cpdef", keyword, &cp))
			{
				candidate = skipTypeDecl (cp, &is_class);
				if (candidate)
				{
					found = TRUE;
					cp = candidate;
				}
			}

			if (found)
			{
				boolean is_parent_class;

				is_parent_class =
					constructParentString(nesting_levels, indent, parent);

				if (is_class)
					parseClass (cp, name, parent, is_parent_class);
				else
					parseFunction(cp, name, parent, is_parent_class);

				addNestingLevel(nesting_levels, indent, name, is_class);
			}
			continue;
		}
		/* Find and parse namespace releated elements */
		if (parseNamespace(line))
			continue;

		/* If the current line contains
		   an open parenthesis skip lines till its associated
		   close parenthesis:

		   foo (...
		   ... ) */
		skipParens (line);
	}
	/* Clean up all memory we allocated. */
	vStringDelete (parent);
	vStringDelete (name);
	vStringDelete (continuation);
	nestingLevelsFree (nesting_levels);
}

extern parserDefinition *PythonParser (void)
{
	static const char *const extensions[] = { "py", "pyx", "pxd", "pxi" ,"scons",
											  NULL };
	static const char *const aliases[] = { "python[23]*", "scons",
										   NULL };
	parserDefinition *def = parserNew ("Python");
	def->kinds = PythonKinds;
	def->kindCount = ARRAY_SIZE (PythonKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->parser = findPythonTags;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4: */
