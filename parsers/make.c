/*
*   Copyright (c) 2000-2005, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for makefiles.
*
*   References:
*   - https://www.gnu.org/software/make/manual/html_node/index.html
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>

#include "make.h"

#include "debug.h"
#include "entry.h"
#include "kind.h"
#include "numarray.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "strlist.h"
#include "vstring.h"
#include "xtag.h"

#include "cpreprocessor.h"

/*
*   DATA DEFINITIONS
*/
typedef enum {
	K_MACRO, K_TARGET, K_INCLUDE,
} makeKind;

typedef enum {
	R_INCLUDE_GENERIC,
	R_INCLUDE_OPTIONAL,
} makeMakefileRole;

static roleDefinition MakeMakefileRoles [] = {
	{ true, "included", "included" },
	{ true, "optional", "optionally included"},
};

static kindDefinition MakeKinds [] = {
	{ true, 'm', "macro",  "macros"},
	{ true, 't', "target", "targets"},
	{ true, 'I', "makefile", "makefiles",
	  .referenceOnly = true, ATTACH_ROLES(MakeMakefileRoles)},
};

typedef enum {
	X_CPP_DEF,
} makeXtag;

static xtagDefinition MakeXtagTable [] = {
	{
		.enabled = false,
		.name = "CppDef",
		.description = "Include FOO in -DFOO as as a name of CPreProcessor macro",
	}
};

/*
*   FUNCTION DEFINITIONS
*/

static int nextChar (void)
{
	int c = getcFromInputFile ();
	if (c == '\\')
	{
		c = getcFromInputFile ();
		if (c == '\n')
			c = nextChar ();
	}
	return c;
}

static void skipLineFull (void (* cb) (int, void *), void *cb_data)
{
	int c;
	do
	{
		c = nextChar ();
		if (c == EOF)
			break;
		if (cb)
			cb (c, cb_data);
	}
	while (c != '\n');
	if (c == '\n')
		ungetcToInputFile (c);
}

static void skipLine (void)
{
	skipLineFull (NULL, NULL);
}

static int skipToNonWhite (int c)
{
	while (c != '\n' && isspace (c))
		c = nextChar ();
	return c;
}

static bool isIdentifier (int c)
{
	return (bool)(c != '\0' && (isalnum (c)  ||  strchr (".-_/$(){}%", c) != NULL));
}

static bool isSpecialTarget (vString *const name)
{
	size_t i = 0;
	/* All special targets begin with '.'. */
	if (vStringLength (name) < 1 || vStringChar (name, i++) != '.') {
		return false;
	}
	while (i < vStringLength (name)) {
		char ch = vStringChar (name, i++);
		if (ch != '_' && !isupper ((unsigned char) ch))
		{
			return false;
		}
	}
	return true;
}

static int makeSimpleMakeTag (vString *const name, makeKind kind, int scopeIndex)
{
	if (!isLanguageEnabled (getInputLanguage ()))
		return CORK_NIL;

	tagEntryInfo e;
	initTagEntry (&e, vStringValue (name), kind);
	e.extensionFields.scopeIndex = scopeIndex;
	return makeTagEntry (&e);
}

static void makeSimpleMakeRefTag (const vString* const name, const int kind,
				  int roleIndex, int scopeIndex)
{
	if (!isLanguageEnabled (getInputLanguage ()))
		return;

	tagEntryInfo e;
	initRefTagEntry (&e, vStringValue (name), kind, roleIndex);
	e.extensionFields.scopeIndex = scopeIndex;
	makeTagEntry (&e);
}

static int newTarget (vString *const name, int scopeIndex)
{
	/* Ignore GNU Make's "special targets". */
	if  (isSpecialTarget (name))
	{
		return CORK_NIL;
	}
	return makeSimpleMakeTag (name, K_TARGET, scopeIndex);
}

static int newMacro (vString *const name, bool with_define_directive, bool appending, int scopeIndex)
{
	int r = CORK_NIL;
	subparser *s;

	if (!appending)
		r = makeSimpleMakeTag (name, K_MACRO, scopeIndex);

	foreachSubparser(s, false)
	{
		makeSubparser *m = (makeSubparser *)s;
		enterSubparser(s);
		if (m->newMacroNotify)
			m->newMacroNotify (m, vStringValue(name), with_define_directive, appending, scopeIndex);
		leaveSubparser();
	}

	return r;
}

static void valueFound (vString *const name)
{
	subparser *s;
	foreachSubparser(s, false)
	{
		makeSubparser *m = (makeSubparser *)s;
		enterSubparser(s);
		if (m->valueNotify)
			m->valueNotify (m, vStringValue (name));
		leaveSubparser();
	}
}

static void directiveFound (vString *const name)
{
	subparser *s;
	foreachSubparser (s, false)
	{
		makeSubparser *m = (makeSubparser *)s;
		enterSubparser(s);
		if (m->directiveNotify)
			m->directiveNotify (m, vStringValue (name));
		leaveSubparser();
	}
}

static void newInclude (vString *const name, bool optional, int scopeIndex)
{
	makeSimpleMakeRefTag (name, K_INCLUDE,
			      optional? R_INCLUDE_OPTIONAL: R_INCLUDE_GENERIC, scopeIndex);
}

static bool isAcceptableAsInclude (vString *const name)
{
	if (strcmp (vStringValue (name), "$") == 0)
		return false;
	return true;
}

static void readIdentifier (const int first, vString *const id)
{
	int depth = 0;
	int c = first;
	vStringClear (id);
	while (isIdentifier (c) || (depth > 0 && c != EOF && c != '\n'))
	{
		if (c == '(' || c == '{')
			depth++;
		else if (depth > 0 && (c == ')' || c == '}'))
			depth--;
		vStringPut (id, c);
		c = nextChar ();
	}
	ungetcToInputFile (c);
}

static void endTargets (intArray *targets, unsigned long lnum)
{
	for (unsigned int i = 0; i < intArrayCount (targets); i++)
	{
		int cork_index = intArrayItem (targets, i);
		setTagEndLineToCorkEntry (cork_index, lnum);
	}
	intArrayClear (targets);
}

static bool isTheLastTargetOnTheSameLine (intArray *current_targets,
										  unsigned long line)
{
	if (!intArrayIsEmpty (current_targets))
	{
		int r = intArrayLast (current_targets);
		tagEntryInfo *e = getEntryInCorkQueue (r);
		if (e && e->lineNumber == line)
			return true;
	}

	return false;
}

/* valueTracker is for extracting FOO in -DFOO. */
struct valueTracker
{
	enum {VT_LOOKING_FOR_D, VT_AFTER_D} state;
	vString *value;				/* NULL: valueTracker is disabled. */
	int leftSideIndex;
	langType cpp;
};

static vString *extractSignature (const char *input, size_t end)
{
	vString *sig = vStringNewInit ("(");
	char c = 0;

	for (size_t j = 0; j < end; j++)
	{
		c = input[j];
		vStringPut (sig, c);
		if (c == ')')
			break;
	}

	if (c != ')')
	{
		/* Incomplete. */
		vStringDelete (sig);
		sig = NULL;
	}

	return sig;
}

static void valueTrackerEval (struct valueTracker *vt)
{
	if (!vt->value)
		return;

	size_t len = vStringLength (vt->value);

	if ((vt->state == VT_AFTER_D && len > 0) ||
		((vt->state == VT_LOOKING_FOR_D &&
		  len > 2 && strncmp("-D", vStringValue (vt->value), 2) == 0)))
	{
		vString *d = vStringNew ();
		vString *sig = NULL;
		for (size_t i = (vt->state == VT_AFTER_D? 0: 2); i < len; i++)
		{
			int c = vStringChar (vt->value, i);

			if (c == '\'' || c == '"')
				continue;
			if (isspace(c) || c == '=')
				break;

			if (c == '(')
			{
				Assert(sig == NULL);
				sig = extractSignature (vStringValue (vt->value) + i + 1, len - i + 1);
				break;
			}

			vStringPut (d, c);
		}
		if (!vStringIsEmpty (d))
		{
			tagEntryInfo e;
			initForeignTagEntry (&e, vStringValue (d), vt->cpp, CPREPRO_MACRO);
			markTagExtraBit (&e, MakeXtagTable[X_CPP_DEF].xtype);
			if (sig)
				e.extensionFields.signature = vStringValue(sig);
			makeTagEntry (&e);
		}
		vStringDelete (sig);	/* NULL is acceptable. */
		vStringDelete (d);

		vt->state = VT_LOOKING_FOR_D;
	}
	else if (len == 2 && strcmp("-D", vStringValue (vt->value)) == 0)
		vt->state = VT_AFTER_D;
}

static void valueTrackerFlush (struct valueTracker *vt)
{
	if (!vt->value)
		return;

	if (vStringIsEmpty (vt->value))
		return;

	valueTrackerEval (vt);
	vStringClear (vt->value);
	vt->state = VT_LOOKING_FOR_D;
}

static void valueTrackerUpdateLeftSideIndex (struct valueTracker *vt, int corkIndex)
{
	if (!vt->value)
		return;

	if (vt->leftSideIndex != corkIndex)
	{
		valueTrackerFlush (vt);
		vt->leftSideIndex = corkIndex;
	}
}

static void valueTrackerPut (struct valueTracker *vt, int c)
{
	if (!vt->value)
		return;

	if (isspace(c))
	{
		if (!vStringIsEmpty (vt->value))
		{
			valueTrackerEval (vt);
			vStringClear (vt->value);
		}
		return;
	}
	vStringPut (vt->value, c);
}

static void valueTrackerPutAsCallback (int c, void *vt)
{
	valueTrackerPut ((struct valueTracker *)vt, c);
}

static void valueTrackerCat (struct valueTracker *vt, vString *vstr)
{
	if (!vt->value)
		return;

	for (size_t i = 0; i < vStringLength (vstr); i++)
	{
		unsigned char c = vStringChar (vstr, i);
		valueTrackerPut (vt, c);
	}
}

static void valueTrackerInit (struct valueTracker *vt)
{
	bool enabled = isXtagEnabled (MakeXtagTable[X_CPP_DEF].xtype);
	vt->state = VT_LOOKING_FOR_D;
	vt->leftSideIndex = CORK_NIL;
	vt->value = enabled? vStringNew(): NULL;
	vt->cpp = enabled? getNamedLanguage ("CPreProcessor", 0): LANG_IGNORE;
}

static void valueTrackerFini (struct valueTracker *vt)
{
	if (!vt->value)
		return;

	valueTrackerFlush (vt);
	vStringDelete (vt->value);
}

/*
 * Naming
 *
 *   X = ...
 *
 * We refer to X as a macro or a variable macro (varmac) here.
 *
 *   define M
 *    ...
 *   endef
 *
 * We refer to M as a macro or a macro definition (macdef) here.
 */
static void findMakeTags0 (void)
{
	/* MAINLY for tracking the candidates of MULTIPLE targets like:
	 *
	 * clean distclean install:
	 *     ...
	 */
	stringList *identifiers = stringListNew ();

	/* parsing the right side of =, :=, +=, ?=, !=
	 *
	 * Nothing to do with targets or macdefs.
	 */
	bool in_varmac_value  = false;

	/* For tracking a tag for a macdef. */
	int  current_macdef = CORK_NIL;

	/* For tracking tags for (multiple) targets.
	 */
	intArray *current_targets = intArrayNew ();

	/* Reading char can be a part of macro name.
	 * In the other words, we are "not in a recipe". */
	bool macro_possible = true;

	/* A char just read*/
	int c;

	/* '\n' seen. */
	bool newline = true;

	/* += seen. */
	bool appending = false;

	/* Gathering uninteresting parts of input to extract -DCPP_MACRO. */
	struct valueTracker value_tracker;
	valueTrackerInit (&value_tracker);

	while ((c = nextChar ()) != EOF)
	{
		if (newline)
		{
			if (!intArrayIsEmpty (current_targets))
			{
				if (c == '\t' || (c = skipToNonWhite (c)) == '#')
				{
					valueTrackerPut(&value_tracker, c == '\t'? '\t': ' ');
					/* skip rule or comment */
					if (c == '#')
						skipLine ();
					else
						skipLineFull (valueTrackerPutAsCallback, &value_tracker);
					c = nextChar ();
				}
				else if (c != '\n')
					endTargets (current_targets, getInputLineNumber () - 1);
			}
			else if (in_varmac_value)
				in_varmac_value = false;

			stringListClear (identifiers);
			macro_possible = intArrayIsEmpty (current_targets);
			newline = false;
		}
		if (c == '\n')
		{
			valueTrackerPut (&value_tracker, '\n');
			newline = true;
		}
		else if (isspace (c))
		{
			valueTrackerPut (&value_tracker, ' ');
			continue;
		}
		else if (c == '#')
			skipLine ();
		else if (macro_possible && (c == '?' || c == '!'))
		{
			c = nextChar ();
			ungetcToInputFile (c);
			macro_possible = (c == '=');
		}
		else if (macro_possible && c == '+')
		{
			c = nextChar ();
			ungetcToInputFile (c);
			macro_possible = (c == '=');
			appending = true;
		}
		else if ((! in_varmac_value) && macro_possible && c == ':' &&
				 stringListCount (identifiers) > 0)
		{
			c = nextChar ();
			ungetcToInputFile (c);
			if (c != '=')
			{
				unsigned int i;
				for (i = 0; i < stringListCount (identifiers); i++)
				{
					int r = newTarget (stringListItem (identifiers, i), current_macdef);
					if (r != CORK_NIL)
					{
						intArrayAdd (current_targets, r);
						valueTrackerUpdateLeftSideIndex (&value_tracker, r);
					}
				}
				stringListClear (identifiers);
			}
		}
		else if (macro_possible && c == '=' &&
				 stringListCount (identifiers) > 0
				 && !in_varmac_value)
		{
			int r = newMacro (stringListItem (identifiers, 0), false, appending, current_macdef);
			valueTrackerUpdateLeftSideIndex (&value_tracker, r);
			stringListClear (identifiers);

			in_varmac_value = true;
			unsigned long curline = getInputLineNumber ();
			unsigned long adj = isTheLastTargetOnTheSameLine (current_targets,
															  curline)? 0: 1;
			endTargets (current_targets, curline - adj);
			appending = false;
		}
		else if (macro_possible && isIdentifier (c))
		{
			vString *name = vStringNew ();
			readIdentifier (c, name);
			stringListAdd (identifiers, name);

			if (in_varmac_value)
			{
				valueFound(name);
				valueTrackerCat (&value_tracker, name);
			}

			if (stringListCount (identifiers) == 1)
			{
				if ((current_macdef != CORK_NIL) && ! strcmp (vStringValue (name), "endef"))
				{
					setTagEndLineToCorkEntry (current_macdef, getInputLineNumber ());
					current_macdef = CORK_NIL;
					stringListClear (identifiers);
				}
				else if (in_varmac_value && current_macdef != CORK_NIL)
					skipLineFull (valueTrackerPutAsCallback, &value_tracker);
				else if (! strcmp (vStringValue (name), "define"))
				{
					c = skipToNonWhite (nextChar ());
					vStringClear (name);
					/* all remaining characters on the line are the name -- even spaces */
					while (c != EOF && c != '\n')
					{
						vStringPut (name, c);
						c = nextChar ();
					}
					if (c == '\n')
						ungetcToInputFile (c);
					vStringStripTrailing (name);

					current_macdef = newMacro (name, true, false, CORK_NIL);
					stringListClear (identifiers);
					valueTrackerUpdateLeftSideIndex (&value_tracker, current_macdef);
				}
				else if (! strcmp (vStringValue (name), "export")
						 || ! strcmp (vStringValue (name), "override"))
					stringListClear (identifiers);
				else if (! strcmp (vStringValue (name), "include")
					 || ! strcmp (vStringValue (name), "sinclude")
					 || ! strcmp (vStringValue (name), "-include"))
				{
					bool optional = (vStringValue (name)[0] == 'i')? false: true;
					while (1)
					{
						c = skipToNonWhite (nextChar ());
						readIdentifier (c, name);
						vStringStripTrailing (name);
						if (!vStringIsEmpty (name) && isAcceptableAsInclude(name))
						{
							newInclude (name, optional, current_macdef);
							valueTrackerUpdateLeftSideIndex (&value_tracker, CORK_NIL);
						}

						/* non-space characters after readIdentifier() may
						 * be rejected by the function:
						 * e.g.
						 * include $*
						 *
						 * Here, remove such characters from input stream.
						 */
						do
							c = nextChar ();
						while (c != EOF && c != '\n' && (!isspace (c)));
						if (c == '\n')
							ungetcToInputFile (c);

						if (c == EOF || c == '\n')
							break;
					}
					stringListClear (identifiers);
				}
				else
					directiveFound (name);
			}
		}
		else
		{
			valueTrackerPut (&value_tracker, c);
			macro_possible = false;
		}
	}

	endTargets (current_targets, getInputLineNumber ());

	valueTrackerFini (&value_tracker);

	intArrayDelete (current_targets);
	stringListDelete (identifiers);
}

static void findMakeTags (void)
{

	subparser *sub = getSubparserRunningBaseparser();
	if (sub)
		chooseExclusiveSubparser (sub, NULL);

	findMakeTags0 ();
}

extern parserDefinition* MakefileParser (void)
{
	static const char *const patterns [] = { "[Mm]akefile", "GNUmakefile", NULL };
	static const char *const extensions [] = { "mak", "mk", NULL };
	static const char *const aliases [] = {
		/* the mode name in emacs */
		"makefile",
		NULL };

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_FOREIGNER, "CPreProcessor", NULL },
	};

	parserDefinition* const def = parserNew ("Make");

	def->versionCurrent = 1;
	def->versionAge = 1;

	def->kindTable      = MakeKinds;
	def->kindCount  = ARRAY_SIZE (MakeKinds);
	def->patterns   = patterns;
	def->extensions = extensions;
	def->aliases = aliases;
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);
	def->xtagTable = MakeXtagTable;
	def->xtagCount = ARRAY_SIZE (MakeXtagTable);
	def->parser     = findMakeTags;
	def->useCork = CORK_QUEUE;
	return def;
}
