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
#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "trace.h"

#include "dependency.h"
#include "autoconf.h"

typedef enum {
	K_TAG,
	K_MACOR,
	K_PACKAGE,
	K_GLOBAL,
	K_PATCH,
} rpmSpecKind;

enum rpmSpecMacroRole {
	R_MACRO_UNDEF,
};
typedef int rpmSpecMacroRole; /* to allow ROLE_INDEX_* */

static roleDefinition RpmSpecMacroRoles [] = {
	{ true, "undef", "undefined" },
};

enum rpmSpecPatchRole {
	R_PATCH_DECL,
};

static roleDefinition RpmSpecPatchRoles [] = {
	{ true, "decl", "declared for applying later" },
};

static scopeSeparator RpmSpecPackageSeparators [] = {
	{ K_PACKAGE , "-" },
};

static kindDefinition RpmSpecKinds[] = {
	{ true, 't', "tag", "tags" },
	{ true, 'm', "macro", "macros",
	  .referenceOnly = false, ATTACH_ROLES(RpmSpecMacroRoles) },
	{ true, 'p', "package", "packages",
	  ATTACH_SEPARATORS(RpmSpecPackageSeparators) },
	{ true, 'g', "global", "global macros" },
	{ true, 'p', "patch", "patch files",
	  .referenceOnly = true, ATTACH_ROLES(RpmSpecPatchRoles) },
};

struct macro_cb_data {
	rpmSpecKind kindex;
	rpmSpecMacroRole rindex;
};

struct rpmSpecCtx {
	bool rejecting;
	struct macro_cb_data macro;
	struct macro_cb_data global;
	struct macro_cb_data undef;
	int package_index;
	int macro_index;
	bool in_configure;
} rpmSpecCtx;


static bool is_line_continued(const char *line)
{
	size_t len = strlen (line);

	if (len == 0)
		return false;

	return ((line[len - 1] == '\\')
		|| ((len >= 2) && (line[len - 1] == '\n') && (line[len - 2] == '\\')))? true: false;
}

static void scan_configure_options (const char *line)
{
	struct configure_opt_prefix {
		const char *prefix;
		int kindIndex, roleIndex;
	} prefixes[] = {
		{.prefix = "--with-",
		 .kindIndex = AUTOCONF_OPTWITH_KIND,
		 .roleIndex = AUTOCONF_OPTWITH_CMDLINE_ROLE},
		{.prefix = "--without-",
		 .kindIndex = AUTOCONF_OPTWITH_KIND,
		 .roleIndex = AUTOCONF_OPTWITH_CMDLINE_ROLE},
		{.prefix = "--enable-",
		 .kindIndex = AUTOCONF_OPTENABLE_KIND,
		 .roleIndex = AUTOCONF_OPTENABLE_CMDLINE_ROLE},
		{.prefix = "--disable-",
		 .kindIndex = AUTOCONF_OPTENABLE_KIND,
		 .roleIndex = AUTOCONF_OPTENABLE_CMDLINE_ROLE},
		{.prefix = NULL,},
	};
	vString *name = vStringNew ();

	for (struct configure_opt_prefix *prefix = &(prefixes [0]);
		 prefix->prefix;
		 prefix++)
	{
		const char *tmp = line;

		while ((tmp = strstr (tmp, prefix->prefix)))
		{
			tmp += strlen(prefix->prefix);
			while (*tmp)
			{
				if (isspace (*tmp) || iscntrl (*tmp)
					|| *tmp == '=' || *tmp == '\\')
					break;
				vStringPut(name, *tmp);
				tmp++;
			}
			if (vStringLength (name) > 0)
			{
				tagEntryInfo e;

				initForeignRefTagEntry (&e,
										vStringValue (name),
										getNamedLanguage ("Autoconf", 0),
										prefix->kindIndex,
										prefix->roleIndex);
				makeTagEntry (&e);
				vStringClear (name);
			}
			tmp++;
		}
	}
	vStringDelete (name);
}

static bool found_configure_cb (const char *line,
							const regexMatch *matches,
							unsigned int count,
							void *userData)
{
	struct rpmSpecCtx *ctx = (struct rpmSpecCtx *)userData;
	ctx->rejecting = (line && is_line_continued (line));
	ctx->in_configure = true;
	scan_configure_options (line);
	return true;
}

static bool found_macro_cb_full (const char *line,
								 const regexMatch *matches,
								 unsigned int count,
								 bool global,
								 bool undef,
								 void *userData)
{
	struct rpmSpecCtx *ctx = (struct rpmSpecCtx *)userData;
	struct macro_cb_data *data;

	ctx->rejecting = (line && is_line_continued (line));

	TRACE_PRINT("Line %04d continuation: %d",
				getInputLineNumber(), ctx->rejecting);

	if (undef)
		data = &ctx->undef;
	else if (global)
		data = &ctx->global;
	else
		data = &ctx->macro;

	if (count > 0)
	{
		vString *signature = ((count > 1) && (matches[2].length > 0))? vStringNew(): NULL;
		vString *name = vStringNew ();
		tagEntryInfo tag;

		if (signature)
			vStringNCopyS (signature, line + matches[2].start, matches[2].length);
		vStringNCopyS (name, line + matches[1].start, matches[1].length);

		if (data->rindex == ROLE_DEFINITION_INDEX)
			initTagEntry (&tag, vStringValue (name), data->kindex);
		else
			initRefTagEntry (&tag, vStringValue (name), data->kindex, data->rindex);

		if (signature)
			tag.extensionFields.signature = vStringValue (signature);

		if (!ctx->rejecting)
		{
			/* The line is not continued. Let's record the endLine now. */
			tag.extensionFields.endLine = getInputLineNumber();
		}

		int cork_index = makeTagEntry (&tag);
		if (data->rindex == ROLE_DEFINITION_INDEX && ctx->rejecting)
		{
			/* The line is continued. Let's record the cork index
			   for attaching endLine field later. */
			ctx->macro_index = cork_index;
		}

		vStringDelete (name);
		if (signature)
			vStringDelete (signature);
	}
	return true;
}

static bool found_macro_cb (const char *line,
							const regexMatch *matches,
							unsigned int count,
							void *userData)
{
	return found_macro_cb_full(line, matches, count, false, false, userData);
}

static bool found_global_cb (const char *line,
			    const regexMatch *matches,
			    unsigned int count,
			    void *userData)
{
	return found_macro_cb_full(line, matches, count, true, false, userData);
}

static bool found_undef_cb (const char *line,
			    const regexMatch *matches,
			    unsigned int count,
			    void *userData)
{
	return found_macro_cb_full(line, matches, count, false, true, userData);
}


static bool alldigits (const char * str)
{
	while (isdigit ((int)*str))
		str++;

	if (*str == '\0')
		return true;
	return false;
}

static bool found_tag_cb (const char *line,
			  const regexMatch *matches,
			  unsigned int count,
			  void *userData)
{
	if (count > 0)
	{
		vString *name = vStringNew ();
		vStringNCopyS (name, line + matches[1].start, matches[1].length);
		makeSimpleTag (name, K_TAG);

		if (count > 1)
		{
			if (strcasecmp (vStringValue (name), "name") == 0)
			{
				vString *package = vStringNew ();
				vStringNCopyS (package, line + matches[2].start, matches[2].length);
				((struct rpmSpecCtx *)userData)->package_index
					= makeSimpleTag (package, K_PACKAGE);
				vStringDelete (package);
			}
			else if (strncasecmp (vStringValue (name), "patch", 5) == 0
					 && alldigits (vStringValue (name) + 5))
			{
				vString *patch = vStringNew ();
				vStringNCopyS (patch, line + matches[2].start, matches[2].length);
				makeSimpleRefTag (patch, K_PATCH, R_PATCH_DECL);
				vStringDelete (patch);
			}
		}
		vStringDelete (name);
	}
	return true;
}

static bool found_package_cb (const char *line,
			      const regexMatch *matches,
			      unsigned int count,
			      void *userData)
{
	if (count > 0)
	{
		vString *name = vStringNew ();
		tagEntryInfo tag;

		vStringNCopyS (name, line + matches[2].start, matches[2].length);
		initTagEntry (&tag, vStringValue (name), K_PACKAGE);
		tag.extensionFields.scopeIndex = ((struct rpmSpecCtx *)userData)->package_index;
		makeTagEntry (&tag);
		vStringDelete (name);
	}
	return true;
}

static bool check_line_continuation (const char *line,
			      const regexMatch *matches,
			      unsigned int count,
			      void *userData)
{
	struct rpmSpecCtx *ctx = (struct rpmSpecCtx *)userData;
	bool rejecting = ctx->rejecting;
	ctx->rejecting = (line && is_line_continued (line));

	TRACE_PRINT("Line %04d continuation: %d -> %d",
				getInputLineNumber(), rejecting, ctx->rejecting);

	tagEntryInfo *e = getEntryInCorkQueue (ctx->macro_index);
	if (rejecting && (!ctx->rejecting) && e)
	{
		e->extensionFields.endLine = getInputLineNumber();
		ctx->macro_index = CORK_NIL;
	}
	else if (rejecting && ctx->in_configure)
	{
		scan_configure_options (line);
		if (!ctx->rejecting)
			ctx->in_configure = false;
	}

	return true;
}

static void findRpmSpecTags (void)
{
	rpmSpecCtx.rejecting = false;
	rpmSpecCtx.macro = (struct macro_cb_data){.kindex = K_MACOR, .rindex = ROLE_DEFINITION_INDEX};
	rpmSpecCtx.global = (struct macro_cb_data){K_GLOBAL, ROLE_DEFINITION_INDEX};
	rpmSpecCtx.undef = (struct macro_cb_data){K_MACOR,  R_MACRO_UNDEF};
	rpmSpecCtx.package_index = CORK_NIL;
	rpmSpecCtx.macro_index = CORK_NIL;
	rpmSpecCtx.in_configure = false;

	findRegexTags ();
}

static void initializeRpmSpecParser (langType language)
{
	addLanguageCallbackRegex (language,  "^([A-Za-z_][A-Za-z_0-9()]+)[ \t]*:[ \t]*([^ \t]*)",
			  "{exclusive}", found_tag_cb, &rpmSpecCtx.rejecting, &rpmSpecCtx);
	addLanguageCallbackRegex (language, "^%configure",
			  "{exclusive}", found_configure_cb, &rpmSpecCtx.rejecting, &rpmSpecCtx);
	addLanguageCallbackRegex (language, "^%define[ \t]+([A-Za-z_][A-Za-z_0-9]+)(\\([^)]+\\))?",
			  "{exclusive}", found_macro_cb, &rpmSpecCtx.rejecting, &rpmSpecCtx);
	addLanguageCallbackRegex (language, "^%undef[ \t]+([A-Za-z_][A-Za-z_0-9]+)",
			  "{exclusive}", found_undef_cb, &rpmSpecCtx.rejecting, &rpmSpecCtx);
	addLanguageCallbackRegex (language, "^%global[ \t]+([A-Za-z_][A-Za-z_0-9]+)(\\([^)]+\\))?",
			  "{exclusive}", found_global_cb, &rpmSpecCtx.rejecting, &rpmSpecCtx);
	addLanguageCallbackRegex (language, "^%package[ \t]+(-n[ \t]+)?([A-Za-z_][A-Za-z_0-9-]+)",
			  "{exclusive}", found_package_cb, &rpmSpecCtx.rejecting, &rpmSpecCtx);
	addLanguageCallbackRegex (language, "^.*$",
			  "{exclusive}", check_line_continuation, NULL, &rpmSpecCtx);
}

extern parserDefinition* RpmSpecParser (void)
{
	static const char *const extensions [] = { "spec", NULL };
	static const char *const aliases [] = {
		"rpm-spec",				/* the mode name in Emacs */
		NULL };
	parserDefinition* const def = parserNew ("RpmSpec");

	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_FOREIGNER, "Autoconf", NULL },
	};

	def->kindTable = RpmSpecKinds;
	def->kindCount = ARRAY_SIZE (RpmSpecKinds);
	def->extensions = extensions;
	def->aliases = aliases;
	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE (dependencies);
	def->initialize = initializeRpmSpecParser;
	def->parser = findRpmSpecTags;
	def->method     = METHOD_REGEX;
	def->useCork = CORK_QUEUE;
	def->requestAutomaticFQTag = true;
	return def;
}
