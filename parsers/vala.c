/*
 * vala.c
 *
 * Copyright 2008 Abderrahim Kitouni <a.kitouni@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 * MA 02110-1301, USA.
 */

/*
 *   INCLUDE FILES
 */
#include "general.h"  /* must always come first */
#include "parse.h"
#include "read.h"

#include "entry.h"
#include "ctags-vala.h"


CTagsVisitor *visitor;
/* using different data structure because fpos_t isn't available in Vala*/
static void make_ctags_entry (CTagsEntry* entry) {
	tagEntryInfo tag;
	initTagEntry(&tag, entry->name, 0); //TODO: ValaKinds

	tag.lineNumberEntry = TRUE;
	tag.lineNumber = entry->line_number;
	tag.kindIndex = entry->kind;
	//tag.filePosition = entry->filePosition;
	tag.extensionFields.access = entry->access;
	tag.extensionFields.implementation = entry->implementation;
	tag.extensionFields.inheritance = entry->inheritance;
	//tag.extensionFields.scope[0] = entry->scope[0];
	//tag.extensionFields.scope[1] = entry->scope[1];
	tag.extensionFields.typeRef[0] = entry->typeref;
	//tag.extensionFields.returnType = entry->returntype;
	tag.extensionFields.signature = entry->signature;
	makeTagEntry(&tag);
}

static kindDefinition ValaKinds [] = {
	{ true,	 'c', "class",       "Classes" },
	{ true,  's', "struct",      "Structures" },
	{ true,  'i', "interface",   "Interfaces" },
	{ true,  'e', "enum",        "Enumerations" },
	{ true,  'v', "enumvalue",   "Enumeration Values" },
	{ true,  'E', "errordomain", "Error domains" },
	{ true,  'r', "errorcode",   "Error codes" },
	{ true,  'd', "delegate",    "Delegates" },
	{ true,  'S', "signal",      "Signals" },
	{ true,	 'f', "field",       "Fields" },
	{ true,	 'p', "property",    "Properties" },
	{ true,  'm', "method",      "Methods" },
	{ false, 'l', "local",       "Local variables" },
};

static void findValaTags (void) {
	if (visitor == NULL) {
		visitor = ctags_visitor_new();
	}
	ctags_visitor_parse_vala (visitor, getInputFileName(), (CTagsEntryMaker) make_ctags_entry);
}

extern parserDefinition* CTagsValaParser(void) {
	g_type_init();
	static const char *const extensions [] = { "vala", "vapi", NULL };
	parserDefinition* def = parserNew ("Vala");
	def->kindTable  = ValaKinds;
	def->kindCount  = ARRAY_SIZE (ValaKinds);
	def->extensions = extensions;
	def->parser     = findValaTags;
	//def->initialize = initializeJavaParser;
	//def->useCork    = true;
	return def;
}

static void findGenieTags (void) {
	if (visitor == NULL) {
		visitor = ctags_visitor_new();
	}
	ctags_visitor_parse_genie (visitor, getInputFileName(), (CTagsEntryMaker) make_ctags_entry);
}

extern parserDefinition *GenieParser(void) {
	static const char *const extensions [] = { "gs", NULL };
	parserDefinition* def = parserNew ("Genie");
	def->kindTable  = ValaKinds;
	def->kindCount  = ARRAY_SIZE (ValaKinds);
	def->extensions = extensions;
	def->parser     = findGenieTags;
	//def->initialize = initializeJavaParser;
	//def->useCork    = true;
	return def;
}
