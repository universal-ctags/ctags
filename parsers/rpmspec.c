/*
*   Copyright (c) 2016 Masatake YAMATO
*   Copyright (c) 2016 Red Hat, Inc.
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for rpm spec files.
*/

/*
 * TODO
 *
 * 1. Capturing required and provide packages as reference tags
 * 2. Capturing bz numbers and package versions in %changelog section
 * 3. Capturing %configure --enable-FOO --with-BAR
 */
#include "general.h"  /* must always come first */

#include <ctype.h>
#include <stddef.h>
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>  /* declare off_t (not known to regex.h on FreeBSD) */
#endif
#include <regex.h>

#include <string.h>
#include "debug.h"
#include "parse.h"
#include "read.h"
#include "routines.h"

typedef enum {
	K_TAG,
	K_MACOR,
	K_PACKAGE,
	K_GLOBAL,
} rpmSpecKind;

enum rpmSpecMacroRole {
	R_MACRO_UNDEF,
};
typedef int rpmSpecMacroRole; /* to allow ROLE_INDEX_* */;

static roleDesc RpmSpecMacroRoles [] = {
	{ TRUE, "undef", "undefined" },
};

static scopeSeparator RpmSpecPackageSeparators [] = {
	{ 'p'          , "-" },
};

static kindOption RpmSpecKinds[] = {
	{ TRUE, 't', "tag", "tags" },
	{ TRUE, 'm', "macro", "macros",
	  .referenceOnly = FALSE, ATTACH_ROLES(RpmSpecMacroRoles) },
	{ TRUE, 'p', "package", "packages",
	  ATTACH_SEPARATORS(RpmSpecPackageSeparators) },
	{ TRUE, 'g', "global", "global macros" },
};


static boolean rejecting;

struct macro_cb_data {
	rpmSpecKind kindex;
	rpmSpecMacroRole rindex;
};

static boolean is_line_continued(const char *line)
{
	size_t len = strlen (line);
	Assert (len > 0);

	return ((line[len - 1] == '\\')
		|| ((len >= 2) && (line[len - 1] == '\n') && (line[len - 2] == '\\')))? TRUE: FALSE;
}

static void found_macro_cb (const char *line,
			    const regexMatch *matches,
			    unsigned int count,
			    void *uesrData)
{
	struct macro_cb_data *data = uesrData;

	if (count > 0)
	{
		vString *signature = ((count > 1) && (matches[2].length > 0))? vStringNew(): NULL;
		vString *name = vStringNew ();
		tagEntryInfo tag;

		if (signature)
			vStringNCopyS (signature, line + matches[2].start, matches[2].length);
		vStringNCopyS (name, line + matches[1].start, matches[1].length);

		if (data->rindex == ROLE_INDEX_DEFINITION)
			initTagEntry (&tag, vStringValue (name), &(RpmSpecKinds[data->kindex]));
		else
			initRefTagEntry (&tag, vStringValue (name), &(RpmSpecKinds[data->kindex]), data->rindex);

		if (signature)
			tag.extensionFields.signature = vStringValue (signature);

		/* Skip the definition */
		while (line && is_line_continued (line))
		{
			rejecting = TRUE;
			line = (const char *)readLineFromInputFile ();
		}
		rejecting = FALSE;

		tag.extensionFields.endLine = getInputLineNumber();

		makeTagEntry (&tag);

		vStringDelete (name);
		if (signature)
			vStringDelete (signature);
	}
}

static void found_tag_cb (const char *line,
			  const regexMatch *matches,
			  unsigned int count,
			  void *userData)
{
	if (count > 0)
	{
		vString *name = vStringNew ();
		vStringNCopyS (name, line + matches[1].start, matches[1].length);
		makeSimpleTag (name, RpmSpecKinds, K_TAG);

		if (count > 1)
		{
			if (strcasecmp (vStringValue (name), "name") == 0)
			{
				vString *package = vStringNew ();
				vStringNCopyS (package, line + matches[2].start, matches[2].length);
				*((int *)userData) = makeSimpleTag (package, RpmSpecKinds, K_PACKAGE);
				vStringDelete (package);
			}
		}
		vStringDelete (name);
	}
}

static void found_package_cb (const char *line,
			      const regexMatch *matches,
			      unsigned int count,
			      void *userData)
{
	if (count > 0)
	{
		vString *name = vStringNew ();
		tagEntryInfo tag;

		vStringNCopyS (name, line + matches[1].start, matches[1].length);
		initTagEntry (&tag, vStringValue (name), RpmSpecKinds + K_PACKAGE);
		tag.extensionFields.scopeIndex = *(int *)userData;
		makeTagEntry (&tag);
		vStringDelete (name);
	}
}

static void initializeRpmSpecParser (langType language)
{
	static int package_index = CORK_NIL;
	rejecting = FALSE;

	static struct macro_cb_data macro  = {K_MACOR,  ROLE_INDEX_DEFINITION};
	static struct macro_cb_data global = {K_GLOBAL, ROLE_INDEX_DEFINITION};
	static struct macro_cb_data undef  = {K_MACOR,  R_MACRO_UNDEF};

	addCallbackRegex (language,  "^([A-Za-z_][A-Za-z_0-9()]+)[ \t]*:[ \t]*([^ \t]*)",
			  "{exclusive}", found_tag_cb, &rejecting, &package_index);
	addCallbackRegex (language, "^%define[ \t]+([A-Za-z_][A-Za-z_0-9]+)(\\([^)]+\\))?",
			  "{exclusive}", found_macro_cb, &rejecting, &macro);
	addCallbackRegex (language, "^%undef[ \t]+([A-Za-z_][A-Za-z_0-9]+)",
			  "{exclusive}", found_macro_cb, &rejecting, &undef);
	addCallbackRegex (language, "^%global[ \t]+([A-Za-z_][A-Za-z_0-9]+)(\\([^)]+\\))?",
			  "{exclusive}", found_macro_cb, &rejecting, &global);
	addCallbackRegex (language, "^%package[ \t]+([A-Za-z_][A-Za-z_0-9-]+)",
			  "{exclusive}", found_package_cb, &rejecting, &package_index);
}

extern parserDefinition* RpmSpecParser (void)
{
	static const char *const extensions [] = { "spec", NULL };
	parserDefinition* const def = parserNew ("RpmSpec");
	def->kinds = RpmSpecKinds;
	def->kindCount = ARRAY_SIZE (RpmSpecKinds);
	def->extensions = extensions;
	def->initialize = initializeRpmSpecParser;
	def->method     = METHOD_NOT_CRAFTED|METHOD_REGEX;
	def->useCork = TRUE;
	def->requestAutomaticFQTag = TRUE;
	return def;
}
