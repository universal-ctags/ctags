#ifndef _cxxTag_h_
#define _cxxTag_h_
/*
*   Copyright (c) 2016, Szymon Tomasz Stefanek
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License version 2 or (at your option) any later version.
*
*   This module contains functions for parsing and scanning C++ source files
*/

#include "general.h"

#include "kind.h"
#include "entry.h"

#include "cxx_token.h"

enum CXXTagKind
{
	CXXTagKindCLASS,
	CXXTagKindMACRO,
	CXXTagKindENUMERATOR,
	CXXTagKindFUNCTION,
	CXXTagKindENUM,
	CXXTagKindINCLUDE,
	CXXTagKindLOCAL,
	CXXTagKindMEMBER,
	CXXTagKindNAMESPACE,
	CXXTagKindPROTOTYPE,
	CXXTagKindSTRUCT,
	CXXTagKindTYPEDEF,
	CXXTagKindUNION,
	CXXTagKindVARIABLE,
	CXXTagKindEXTERNVAR,
	CXXTagKindPARAMETER,
	CXXTagKindLABEL,
	CXXTagKindNAME,
	CXXTagKindUSING
};

kindOption * cxxTagGetKindOptions(void);
int cxxTagGetKindOptionCount(void);
boolean cxxTagKindEnabled(enum CXXTagKind eKindId);

// Begin composing a tag.
// Returns NULL if the tag should *not* be included in the output
// or the tag entry info that can be filled up with extension fields.
// Must be followed by cxxTagCommit() if it returns a non-NULL value.
// The pToken ownership is NOT transferred.
tagEntryInfo * cxxTagBegin(enum CXXTagKind eKindId,CXXToken * pToken);

// Commit the composed tag. Must follow a succesfull cxxTagBegin() call.
void cxxTagCommit(void);

// Same as cxxTagBegin() eventually followed by cxxTagCommit()
void cxxTag(enum CXXTagKind eKindId,CXXToken * pToken);

typedef enum {
	CR_MACRO_UNDEF,
} cMacroRole;

typedef enum {
	CR_HEADER_SYSTEM,
	CR_HEADER_LOCAL,
} cHeaderRole;

#endif //!_cxxTag_h_
