/*
*   Copyright (c) 2024, Masatake YAMATO
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for generating tags for Cargo.toml file.
*
*   - ref. https://doc.rust-lang.org/cargo/index.html
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "x-toml.h"
#include "entry.h"
#include "kind.h"
#include "read.h"
#include "parse.h"
#include "subparser.h"

#include <string.h>

/*
*   DATA DECLARATIONS
*/

typedef enum {
	K_PACKAGE,
#if 0
	K_CRATE,
#endif
} CargoKind;

static kindDefinition CargoKinds[] = {
	{true, 'p', "package", "packages"},
#if 0
	{true, 'c', "crate", "crates"},
#endif
};

struct sCargoSubparser {
	tomlSubparser toml;
};

/*
*   FUNCTION DEFINITIONS
*/
static void valueCallback (tomlSubparser *sub, const char *value, long offset, int corkIndex)
{
	if (value[0] == '\0' || (value[0] == '"' && value[1] == '"'))
		return;

	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
	if (e && strcmp(e->name, "name") == 0
		&& e->extensionFields.scopeIndex != CORK_NIL)
	{
		tagEntryInfo *parent = getEntryInCorkQueue(e->extensionFields.scopeIndex);
		if (parent && strcmp(parent->name, "package") == 0
			&& parent->extensionFields.scopeIndex == CORK_NIL)
		{
			tagEntryInfo pe;
			unsigned long lineNumber = getInputLineNumberForFileOffset(offset);
			MIOPos filePosition = getInputFilePositionForLine (lineNumber);

			/* Dropping double quote chars. */
			vString *package = vStringNewInit (*value == '"'? value + 1: value);
			if (*value == '"')
				vStringChop (package);

			initTagEntry (&pe, vStringValue (package), K_PACKAGE);
			updateTagLine (&pe, lineNumber, filePosition);

			makeTagEntry (&pe);

			vStringDelete (package);
		}
	}
}

#if 0
static void makeTagEntryCallback(subparser *s, const tagEntryInfo *tag, int corkIndex)
{
	tagEntryInfo *e = getEntryInCorkQueue (corkIndex);
	if (e && e->extensionFields.scopeIndex != CORK_NIL && e->kindIndex == TOML_K_QKEY)
	{
		tagEntryInfo *pe = getEntryInCorkQueue (e->extensionFields.scopeIndex);
		if (pe && pe->extensionFields.scopeIndex == CORK_NIL)
		{
			if (strcmp(pe->name, "dependencies") == 0)
				;
			tagEntryInfo ce;

			initTagEntry (&ce, e->name, K_CRATE);
			ce.lineNumber = e->lineNumber;
			ce.filePosition = e->filePosition;

			makeTagEntry (&ce);
		}
	}
}
#endif

static void findCargoTags (void)
{
	scheduleRunningBaseparser (0);
}

extern parserDefinition* CargoParser (void)
{
	static const char *const patterns [] = { "Cargo.toml", NULL };

	static struct sCargoSubparser cargoSubparser = {
		.toml = {
			.subparser = {
				.direction = SUBPARSER_SUB_RUNS_BASE,
#if 0
				.makeTagEntryNotify = makeTagEntryCallback,
#endif
			},
			.valueNotify = valueCallback,
		},
	};
	static parserDependency dependencies [] = {
		[0] = { DEPTYPE_SUBPARSER, "TOML", &cargoSubparser },
	};

	parserDefinition* const def = parserNew ("Cargo");

	def->dependencies = dependencies;
	def->dependencyCount = ARRAY_SIZE(dependencies);
	def->kindTable = CargoKinds;
	def->kindCount  = ARRAY_SIZE (CargoKinds);
	def->patterns   = patterns;
	def->parser     = findCargoTags;
	def->useCork    = CORK_QUEUE;
	return def;
}
